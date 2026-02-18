echo off, clc;
hold off;
clear all;
close all;

sz = 10;
x = linspace(0,1,sz);
y = linspace(0,1,sz);
z = linspace(0,1,sz);
eta_1 = sin(2*pi*x).*cos(2*pi*y).*cos(2*pi*z);
eta_2 = -cos(2*pi*x).*sin(2*pi*y).*cos(2*pi*z);
eta_3 =  zeros(1,sz);
eta_tg = [[eta_1],[eta_2],[eta_3]];


Y = eta_tg;
s = 0;
for p = 1:3
    test = fft(eta_tg(1:sz*p),2*sz+1);
    index = 1;
    for k = -sz:sz
  
            if(k~=0)
                s = s + (abs(k)^(2))*abs(test(index))^2;
                index = index + 1;
            end
            
    end
end
s

