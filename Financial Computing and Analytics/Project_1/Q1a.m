#Question1a
printf("Question 1a\n")
printf("-----------\n")
x1 = input("Enter x1 value\n")
x2 = input("Enter x2 value\n")
x = [x1;x2];

function y = fun(x)
	y = (	-4*x(1)/	(x(1)^2 + x(2)^2 + 2)  );
endfunction


#Calcualte gradient for the given function
function gradient = function_gradient(x)
	a = (4*( x(1)^2 - x(2)^2 - 1 )) / ( (x(1)^2 + x(2)^2 + 1 )^2 );
	b = ( 8*x(1)*x(2) ) / ( (x(1)^2 + x(2)^2 + 1 )^2 );
	gradient = [a;b];
endfunction

#on of iterations
iterations = 10000;
#Settting B value with identity matrix
B = eye(2);
iter = 0;

while( (abs(function_gradient(x)(1)) > 0.09 | abs(function_gradient(x)(2)) > 0.09 ) && iter < iterations )
	previous_x = x;
	Pk = -inv(B) * function_gradient(x);
	
	#line search
	alpha = 0.01;
	i = 0.001;
	while (fun(x+alpha*Pk)<fun(x))
		alpha=alpha+ i;
		x=x+alpha*Pk;
		i = i * 2;
	endwhile
	
	
	%{
	alpha = 0.001;
	for i = 1000:-1:1
		alpha1 = 1/i;
		if(fun(x+(alpha+alpha1)*Pk)<fun(x))
			alpha = alpha1 + alpha;
			x = x + alpha * Pk;
		endif
	endfor
	%}
	
	sk = x - previous_x;
	yk = fun(x) - fun(previous_x);
	#Updation
	if (yk'*Sk>0)
		B=B+(B*sk)*(B*sk)'/(sk'*B*sk)+(yk*yk')/(yk'*sk);
   	else
		n = 1;
	endif
	iter = iter + 1;
endwhile
