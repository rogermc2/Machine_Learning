
with PLplot_Auxiliary; use PLplot_Auxiliary;
with PLplot; use PLplot;

 procedure Simple_Example is
    x : Real_Vector(-10 .. 10);
    y : Real_Vector(-10 .. 10);
 begin
    for i in x'range loop
       x(i) := Long_Float(i);
       y(i) := x(i)**2;
    end loop;
    
    Initialize_PLplot; -- Call this only once.
    Simple_Plot(x, y); -- Make the plot.
    End_PLplot;        -- Call this only once.
    
 end Simple_Example;
