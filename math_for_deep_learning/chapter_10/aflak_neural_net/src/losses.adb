
with Neural_Maths;

package body Losses is

   function Mean_Square_Error (Y_True, Y_Pred : Real_Float_Vector) return Float is
      use Real_Float_Arrays;
   begin
      return 0.5 * Neural_Maths.Mean ((Y_True - Y_Pred) ** 2);

   end Mean_Square_Error;

   --  ------------------------------------------------------------------------

   function Minus_MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                                  return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
   begin
      return Y_Pred - Y_True;

   end Minus_MSE_Derivative;

   --  --------------------------------------------------------------

end Losses;
