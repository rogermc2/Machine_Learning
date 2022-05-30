
package body Test_Common is

   procedure Init (Params : out Stochastic_Optimizers.Parameters_List) is
      use Stochastic_Optimizers;
      I2 : Integer_Array (1 .. 2) := (4, 6);
      I3 : constant Integer_Array (1 .. 3) := (7, 8, 9);
   begin
      Shapes.Append (I2);
      I2 := (6, 8);
      Shapes.Append (I2);
      Shapes.Append (I3);

      for shape in Shapes.First_Index .. Shapes.Last_Index loop
         declare
            Bounds    : constant Integer_Array := Shapes.Element (shape);
            Coeff     : constant
              Real_Float_Matrix (1 .. Bounds (1), 1 .. Bounds (2))
              := (others =>  (others => 0.0));
            Ints      : constant Real_Float_Vector (1 .. Bounds (2)) :=
                          (others => 0.0);
            PR        : Parameters_Record (Bounds (1), Bounds (2));
         begin
            PR.Coeff_Gradients := Coeff;
            PR.Intercept_Grads := Ints;
            Params.Append (PR);
         end;
      end loop;

   end Init;

end Test_Common;
