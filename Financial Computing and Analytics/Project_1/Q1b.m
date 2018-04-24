#Question1a
#Maximum of the function
printf("Question 1b\n");
printf("-----------\n");
x1 = input("Enter x1 value\n");
x2 = input("Enter x2 value\n");
x = [x1;x2];

function y = fun(x)
	y = (	-4*x(1)/	(x(1)^2 + x(2)^2 + 1)  );
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

while( (abs(function_gradient(x)(1)) >0.00001 || abs(function_gradient(x)(2)) > 0.00001 ) && iter < iterations )
	previous_x = x;
	Pk = inv(B) * function_gradient(x);
	
	#line search
	alpha = 0.001;

	while (fun(x+alpha*Pk)>fun(x))
		alpha=alpha+0.0001;
		x=x+alpha*Pk;
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
	if(yk'*sk != 0)
		B=B+(B*sk)*(B*sk)'/(sk'*B*sk)+(yk*yk')/(yk'*sk);
	endif
	iter = iter + 1;
endwhile

printf("The given function is maximum at ")
x