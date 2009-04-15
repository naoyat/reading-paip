(require "./ch07-2")

#;(student '(if the number of customers Tom gets is twice the square of 
			  20 % of number of advertisements he runs |,|
			  and the number of advertisements is 45 |,|
			  then what is the number of customers Tom gets ?))
(student '(if customers Tom gets is twice square of 20 % advertisements he runs |,|
			  and advertisements is 45 |,|
			  then what is customers Tom gets ?))
#|=>
The equations to be solved are:
   customers = (2 * (((20 / 100) * advertisements) * ((20 / 100) * advertisements)))
   advertisements = 45
   what = customers
The solution is:
   what = 162
   customers = 162
   advertisements = 45
|#

(student '(the daily cost of living for a group is the overhead cost plus
			   the running cost for each person times the number of people in
			   the group |.|  this cost for one group equals $ 100 |,|
			   and the number of people in the group is 40 |.|
			   if the overhead cost is 10 times the running cost |,|
			   find the overhead and running cost for each person |.|))
#|=>
The equations to be solved are:
   daily = (overhead + (running * people))
   cost = 100
   people = 40
   overhead = (10 * running)
   to-find-1 = overhead
   to-find-2 = running
The solution is:
   people = 40
   cost = 100
|#
(student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
				  Kelly's IQ minus 80 is Robin's height |.|
				  if Robin is 4 feet tall |,| how old is Fran ?))
#|=>
The equations to be solved are:
   (Fran / Robin) = (Kelly / 2)
   (Kelly - 80) = Robin
   Robin = 4
   how = Fran
The solution is:
   how = 168
   Fran = 168
   Kelly = 84
   Robin = 4
|#
(student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
				  Kelly's IQ minus 80 is Robin's height |.|
				  if Robin is 0 feet tall |,| how old is Fran ?))
#|=>
The equations to be solved are:
   (Fran / Robin) = (Kelly / 2)
   (Kelly - 80) = Robin
   Robin = 0
   how = Fran
The solution is:
   how = 0
   Fran = 0
   Kelly = 80
   Robin = 0
|#
(student '(Fran's age times Robin's height is one half Kelly's IQ |.|
				  Kelly's IQ minus 80 is Robin's height |.|
				  if Robin is 0 feet tall |,| how old is Fran ?))
#|=>
The equations to be solved are:
   (Fran * Robin) = (Kelly / 2)
   (Kelly - 80) = Robin
   Robin = 0
   how = Fran
The solution is:
   how = +inf.0
   Fran = +inf.0
   Kelly = 80
   Robin = 0
|#

