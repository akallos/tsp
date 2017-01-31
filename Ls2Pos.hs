module Main where
import qualified Data.Vector as V
import System.Random
import System.Random.Shuffle
import qualified Data.List as L

type Vertex=(Double,Double)
type EdgeDef=(Int,Int)
type Edge=(Vertex,Vertex)
type Path=[Int]

type CoordinateSet=V.Vector Vertex

from_def::CoordinateSet->EdgeDef->Edge
from_def coords (a,b)=((coords V.!a),(coords V.!b))

side_of_line::Edge->Vertex->Double
side_of_line ((ax,ay),(bx,by)) (cx,cy)=signum $ 
        (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)


crossed'::Edge->Edge->Bool

crossed' e (v1,v2)=(side_of_line e v1 ) /= (side_of_line e v2)

edge_length::Edge->Double
edge_length ((x1,y1), (x2,y2))=sqrt $(dx*dx)+(dy*dy) 
    where
        dx=x2-x1
        dy=y2-y1
path_length::[Int]->CoordinateSet->Double
path_length [] _ =0::Double
path_length [x] _=0::Double
path_length [a,b] coords =edge_length $ from_def coords (a,b)
path_length lst coords=l
    where
        s=(segments lst)++[((head lst),(last lst))]
        s'=map (from_def coords) s
        l=foldr1 (+) $ map (edge_length) s'
        
crossed::CoordinateSet->EdgeDef->EdgeDef->Bool
crossed coords e1 e2=crossed' (from_def coords e1) (from_def coords e2)

decross::[Int]->EdgeDef->EdgeDef->[Int]

decross path (x1,x2) (y1,y2)=s1++(reverse s2)++s3
    where
        (s1,s2_)=splitAt x1 path
        (s2,s3)= splitAt (y2-(length s1)-1) s2_

segments::[Int]->[(Int,Int)]
segments []=[]
segments [a]=[]
segments (p:(a:(ths)))=(p,a):(segments (a:ths))

compare_paths::CoordinateSet->[Int]->[Int]->[Int]
compare_paths coords p1 p2=
    if l1>l2 then p2 else p1
    where 
        l1=path_length p1 coords
        l2=path_length p2 coords

best_cross::[Int]->CoordinateSet->EdgeDef->[Int]
best_cross path coords e=foldr (best_cross') path neighbours 
    where 
        (_,v2)=e
        (_,s2)=splitAt v2 path
        crossed_segments'=[(s,(edge_length (from_def coords s))) |s<-(segments s2),(crossed coords e s) ]
        crossed_segments_sorted=L.sortBy (\(_,x) (_,y)->y`compare`x) crossed_segments'
        crossed_segments=map (fst) crossed_segments_sorted 
        neighbours=map (decross path e) crossed_segments
        best_cross' p1 p2= compare_paths coords p1 p2

            
local_search::[Int]->CoordinateSet->[Int]
local_search path coords=
   if path==best then path
   else local_search best coords 
   where
        s=segments path 
        sols=map (best_cross path coords ) s
        best=foldr (compare_paths coords) path sols
         

permute::[Int]->Int->Int->[Int]
permute path 0 _=path
permute path times seed=permute new_path (times-1) newseed
    where
        g=mkStdGen seed
        [e1,e2]=take 2 $ randomRs (0,(length path)-2) g
        (newseed,_)=random g
        s=segments path  
        new_path=decross path (s!!e1) (s!!e2)
        
randomized_local_search::[Int]->CoordinateSet->Int->Int->[Int]
randomized_local_search path _ 0 _=path
randomized_local_search path coords trials seed=randomized_local_search (compare_paths coords sol sol2) coords (trials-1) newseed
    
    where
        sol=local_search path coords
        g=mkStdGen seed
        [ps,newseed]=take 2 $ randoms g 
        mutation=permute path 5 ps
        sol2= local_search mutation coords
        

read_items::Int->IO [(Double,Double)]
read_items n = 
    if n==0 then 
        return [] 
    else do 
        i <- getLine 
        let (v:(w:_))=map (\x->read x::Double) (words i)
        is <- read_items (n-1) 
        return ((v,w):is)


shuffle_with::[Int]->Int->[Int]
shuffle_with p seed=shuffle' p (length p) g
    where
        g=mkStdGen seed
        

randomized_2pos::[Int]->CoordinateSet->Int->Int->[Int]
randomized_2pos path coords trials seed=foldr (compare_paths coords) sol0 sols
    where
        g=mkStdGen seed
        random_seeds=take trials $ randoms g
        random_paths=[(shuffle_with path s)|s<-random_seeds]
        sols=[local_search p coords|p<-random_paths]
        sol0=local_search path coords

main=do
    n<-readLn::IO Int
    nodes<-read_items n
    r<-randomIO::IO Int
    let coords=V.fromList nodes
    let init_path=[0..n-1]
    let sol=randomized_local_search init_path coords 30 r
    putStrLn (show (path_length sol coords)++" 0")
    putStrLn $ unwords $ map (show) sol
