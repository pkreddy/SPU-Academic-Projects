#Minimum of the function b
printf("Min of function b\n");
printf("-----------\n");
x1 = input("Enter x1 value\n");
x2 = input("Enter x2 value\n");
x = [x1;x2];

function y = fun(x)
	y = (	sin(x(1)^2+x(2)^2));
endfunction


#Calcualte gradient for the given function
function gradient = function_gradient(x)
	a = (2*x(1)*cos(x(1)^2+x(2)^2));
	b = (2*x(2)*cos(x(1)^2+x(2)^2));
	gradient = [a;b];
endfunction

#on of iterations
iterations = 10000;
#Settting B value with identity matrix
B = [1,0;0,1];
iter = 0;

while( (abs(function_gradient(x)(1)) >0.00001 || abs(function_gradient(x)(2)) > 0.00001 ) && iter < iterations )
	previous_x = x;
	Pk = -inv(B) * function_gradient(x);
		
  
  #calculating alpha  
	stop = 0;
	alpha = 0.001;
	for i = 1000:-1:1
		alpha1 = 1/i;
		if(fun(x+(alpha+alpha1)*Pk)<fun(x) && stop==0)
			alpha = alpha1 + alpha;
			x = x + alpha * Pk;
		else 
      stop = 1;
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