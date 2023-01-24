doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

length' xs = sum[1|_<-xs]

--keepLowerCase st = [c|c<-st,c 'elem'['a'..'z']]