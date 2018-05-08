printf("evaluating binary tempee for Call Option\n")
function option = Binarytempee(s0,k,sig,r,t,N)
  temp = valE  = valA = zeros(N+1,N+1);
  #temp = valE = ones(N+1,N+1);
  dt = t/N;
  u = exp(sig*sqrt(dt));
  d = exp(-sig*sqrt(dt));
  temp(1,:) = s0*u.^(0:N);
  pu = (exp(r*dt) - d) / (u - d);
  pd = 1 - pu;
  pv = exp(-r*dt);
  u
  d
  
  temp(1,:) = s0*u.^(0:N);
  
  for i=2:(N+1)
    temp(2:i,i) = temp(1:(i-1),i-1)*d;
  endfor
  
  valE(:,N+1) = valA(:,N+1) = max(temp(:,N+1)-k,0);
  #valE(:,N+1) = max(temp(:,N+1)-k,0);
  
  for i=N:-1:1
    valE(1:i,i) = pv * (pu * valE(1:i,i+1) + pd * valE(2:(i+1),i+1));
    tval = pv * (pu * valA(1:i,i+1) + pd * valA(2:(i+1),i+1));
    ex = (temp(1:i,i) - k);
    valA(1:i,i) = max( ex, tval);
  endfor

  option = 0;
    
endfunction

 
#fprint(BT(s0=144,k=150,sig=sqrt(0.1),r=0.02,t=0.25,N=100));
Binarytempee(144,150,sqrt(0.1),0.02,0.25,100);
