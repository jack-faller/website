(post
 "Reciprocal Probability Events More Likely than Expected"
 "2025/01/07 15:57 +0000"
 {just
  Events with a probability x{sup -1} are typically said to be more likely than not after x occurrences.
  A better estimate is 0.7x occurrences.}
 {p
  You will have no doubt encountered this common rule of thumb while working with arbitrary probabilities.
  Those who are more mathematically inclined will eventually wonder why it holds true, and if they try it out will discover that it alarmingly does not.
  For example, an event with probability #(math '(/ 1 137)) has a 50/50 chance of happening after roughly #(format #f "~,2f" (/ (log 0.5) (log (/ 136 137)))) tries.
  I find this by noting that where the probability of the event is a half, the probity of it not happening is also a half.
  Therefore T, the number of times for this to be the case, satisfies
  #(math-block
	'(= (^ (- 1 (/ 1 137)) "T") (/ 1 2)))
  and I find this by computing
  #(math-block
	`(=
	  "T"
	  ((logb (- 1 (/ 1 137))) (/ 1 2))
	  (/ ("ln" 0.5) ("ln" (- 1 (/ 1 137))))
	  (~~ #f ,(string->number (format #f "~,2f" (/ (log 0.5) (log (/ 136 137))))))))
  This number is much lower than you would expect using the common estimation.}
 {p
  What I find more interesting about the estimation that it being wrong is that it's almost right.
  The number of repeats before it is likely seems to grow linearly with the denominator of the probability.
  To determine why this is, let us consider the general formula for such probabilities.
  #(math-block
	'(= "T" (/ ("ln" 0.5) ("ln" (- 1 (^ "x" -1))))))
  What does this approach?
  Well for larger values of x, #(math '(- 1 (^ "x" -1))) approaches 1.
  Around x = 1, #(math '(~~ ("ln" "x") (- "x" 1))).
  So substituting those into the formula:
  #(math-block
	`(~~
	  "T"
	  (/ ("ln" 0.5) (- 1 (- (^ "x" -1) 1)))
	  (= #f (dot (neg ("ln" 0.5)) "x"))
	  (* ,(string->number (format #f "~,2f" (- (log 0.5)))) "x")))}
 {p
  This was my initial approach.
  It's almost right, but logic is flawed.
  You can't go willing-nilly substituting approximations for things unless you check that the error in those approximations approaches zero.
  The actual function that this approaches is
  #(math-block '(~~ "T" (dot (neg ("ln" 0.5)) (- "x" 0.5))))
  which quickly converges though is just a constant (negligible) offset from the other approximation.
  It can be found by instead substituting the first term of a series expansion for ln
  #(math-block
	(let* ((w_1 '(+ (/ (- "x" 1) (+ "x" 1))))
		   (w_n (lambda (n) `(* (/ 1 ,n) (^ ,w_1 ,n)))))
	  `(= ("ln" "x") (* 2 (+ ,w_1 ,(w_n 3) ,(w_n 5) dots)))))
  into the limit of a linear approximation
  #(math-block
	'(~~ ("f" "x")
		 (lim (("h" inf))
			  (+ (* "x" ("f′" "h")) "h" (- #f (/ ("f" "h") ("f′" "h")))))))}
 (let ((denom (lambda (x) `("ln" (- 1 (^ ,x -1))))))
   {p
	First taking the derivative
	#(math-block
	  `(stack
		(= ((/ "d" (* "d" "h")) (/ 1 ,(denom "h")))
		   (* (neg (^ "h" -2)) (^ (- 1 (^ "h" -1)) -1) (^ ,(denom "h") -2)))
		(= #f (* (^ (* (^ "h" 2) (- (^ "h" -1) 1)) -1) (^ ,(denom "h") -2)))
		(= #f (* (^ (- "h" (^ "h" 2)) -1) (^ ,(denom "h") -2)))))
	and noting that this tends to −1 due to h squared dominating the denominator of the fraction.
	Then substituting this into the approximation
	#(math-block
	  `(stack
		(~~ (/ 1 ,(denom "x"))
			(lim
			 (("h" inf))
			 (+ (* "x" ((/ "d" (* "d" "h")) (/ 1 ,(denom "h")))) "h"
				(- #f (/ (* (- "h" (^ "h" 2)) (^ ,(denom "h") 2)) ,(denom "h"))))))
		(= #f (+ (neg "x") (lim (("h" inf)) (+ "h" (* (- (^ "h" 2) "h") ,(denom "h"))))))
		(= #f (+ (neg "x")
				 (lim (("h" inf)) (- "h" (* (- (^ "h" 2) "h") ,(denom "h"))))))
		(= #f (+ (neg "x")
				 (lim (("h" inf))
					  (+ "h" (* (- (^ "h" 2) "h")
								(* 2 (+ (/ (neg (^ "h" -1)) (- 2 (^ "h" -1)))
										(* (/ 1 3) (^ (/ (neg (^ "h" -1))
														 (- 2 (^ "h" -1))) 3))
										dots)))))))

		(= #f (+ (neg "x")
				 (lim (("h" inf))
					  (+ "h" (* (- (^ "h" 2) "h")
								(* 2 (+ (/ (^ "h" -1) (- (^ "h" -1) 2))
										(* (/ 1 3) (^ (/ (^ "h" -1)
														 (- (^ "h" -1) 2)) 3))
										dots)))))))
		(= #f (+ (neg "x")
				 (lim (("h" inf))
					  (+ "h"
						 (/ (- (* 2 (^ "h" 2)) (* 2 "h")) (- 1 (* 2 "h")))
						 (* (/ 2 3) (/ (- (^ "h" 2) "h")
									   (^ (- 1 (* 2 "h")) 3)))
						 dots))))
		(comment #f "as h goes to infinity, the denominator dominates")
		(= #f (+ (neg "x") (lim (("h" inf)) (+ "h" (/ (- (* 2 (^ "h" 2)) (* 2 "h")) (- 1 (* 2 "h")))))))
		(= #f (+ (neg "x") (lim (("h" inf)) (+ "h" (- #f "h") 0.5
										 (/ 0.5 (- 1 (* 2 "h")))))))
		(= #f (- 0.5 "x"))))
	we get very simple result for all that work.})
 {p
  So the more general formula for the number of repeats T to yield a probability P of the event with probability x{sup -1} occurring is
  #(math-block '(= "T" (/ ("ln" (- 1 "P")) ("ln" (- 1 (^ "x" -1))))
				   (~~ #f (dot (neg ("ln" (- 1 "P"))) (- "x" 0.5)))))
  For most values of P, the −0.5 offset of the graph is negligible.
  It only becomes relevant where P is close to 1, in which case the 0.7x estimate from earlier is inaccurate.
  The estimate is also inaccurate for choices of x close to 1, i.e. high certainty events.
  This isn't an issue for reciprocal probabilities, where 2 is the highest value used, but for other values of x between 1 and 2 (corresponding to events with more than a 50% likelihood) the estimation is very inaccurate.
  Luckily its also not very useful for those values as they are very likely after only a single repeat.})
