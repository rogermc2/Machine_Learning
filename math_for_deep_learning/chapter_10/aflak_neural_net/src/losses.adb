
with Neural_Maths;

package body Losses is

   function Binary_Cross_Entropy (Y_True, Y_Pred : Real_Float_Vector) return Float is
      Value : constant Real_Float_Vector := -H_Product (Y_True, Log (Y_Pred));
   begin
      return Neural_Maths.Mean (Value);

   end Binary_Cross_Entropy;

   --  ------------------------------------------------------------------------

   function Binary_Cross_Entropy_Prime (Y_True, Y_Pred : Real_Float_Vector)
                                        return Real_Float_Vector is
      use Real_Float_Arrays;
      True_Neg   : constant Real_Float_Vector := 1.0 - Y_True;
      Pred_Neg   : constant Real_Float_Vector := 1.0 - Y_True;
      Diff       : constant Real_Float_Vector :=
        True_Neg / Pred_Neg -  Y_True / Y_Pred;
   begin
      return Diff / Float (Y_True'Length);

   end Binary_Cross_Entropy_Prime;

   --  ------------------------------------------------------------------------

   function MSE (Y_True, Y_Pred : Real_Float_Vector) return Float is
      use Real_Float_Arrays;
   begin
      return Neural_Maths.Mean ((Y_True - Y_Pred) ** 2);

   end MSE;

   --  ------------------------------------------------------------------------

   function MSE_Prime (Y_True, Y_Pred : Real_Float_Vector)
                       return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
   begin
      return 2.0 * (Y_Pred - Y_True) / Float (Y_True'Length);

   end MSE_Prime;

   --  --------------------------------------------------------------

end Losses;
