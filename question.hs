
module Question where
  
  --ques3 xs = splitAt filter (\x->xs if ) [xs] !! 0

    triangle = [(x,y,z) | x<-[1..10],y<-[1..10],z<-[1..10],x^2 == y^2 + z^2,x>y,y>z]