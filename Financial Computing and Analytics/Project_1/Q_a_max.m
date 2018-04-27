#Maximum of the function a
printf("Max of function a\n");
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

while( (abs(function_gradient(x)(1)) >0.001 || abs(function_gradient(x)(2)) > 0.001 ) && iter < iterations )
	previous_x = x;
	Pk = inv(B) * function_gradient(x);
	
  
  #calculating alpha  
	alpha = 0.001;
	for i = 1000:-1:1
		alpha1 = 1/i;
		if(fun(x+(alpha+alpha1)*Pk)>fun(x))
			alpha = alpha1 + alpha;
			x = x + alpha * Pk;
    endif
	endfor
	
	
	sk = x - previous_x;
	yk = function_gradient(x) - function_gradient(previous_x);
	#Updation
	if (yk'*sk(1) > 0 && yk'*sk(2)>0)
		B=B+(B*sk)*(B*sk)'/(sk'*B*sk)+(yk*yk')/(yk'*sk);
	endif
	iter = iter + 1;
endwhile

printf("number of iterations ran are = \n")
disp(iter)
printf("minimum value is = \n")
disp(fun(x))
printf("The given function is minimum at = \n")
disp(x)