
with Neural_Maths;

package body Losses is

   function Binary_Cross_Entropy (Y_True : Binary_Array;
                                  Y_Pred : Real_Float_List) return Float is
      F_True : constant Real_Float_Vector := To_Real_Float_Vector (Y_True);
      F_Pred : constant Real_Float_Vector := To_Real_Float_Vector (Y_Pred);
      Value  : constant Real_Float_Vector := -H_Product (F_True, Log (F_Pred));
   begin
      return Neural_Maths.Mean (Value);

   end Binary_Cross_Entropy;

   --  ------------------------------------------------------------------------

   function Binary_Cross_Entropy_Prime
     (Y_True : Binary_Array; Y_Pred : Real_Float_List)
      return Real_Float_Vector is
      use Real_Float_Arrays;
      F_True : constant Real_Float_Vector := To_Real_Float_Vector (Y_True);
      F_Pred : constant Real_Float_Vector := To_Real_Float_Vector (Y_Pred);
      True_Neg   : constant Real_Float_Vector := 1.0 - F_True;
      Pred_Neg   : constant Real_Float_Vector := 1.0 - F_Pred;
      Diff       : constant Real_Float_Vector :=
        True_Neg / Pred_Neg -  F_True / F_Pred;
   begin
      return Diff / Float (Y_True'Length);

   end Binary_Cross_Entropy_Prime;

   --  ------------------------------------------------------------------------

   function MSE (Y_True : Binary_Array; Y_Pred : Real_Float_List)
                 return Float is
      use Real_Float_Arrays;
      F_True : constant Real_Float_Vector := To_Real_Float_Vector (Y_True);
      F_Pred : constant Real_Float_Vector := To_Real_Float_Vector (Y_Pred);
   begin
      return Neural_Maths.Mean ((F_True - F_Pred) ** 2);

   end MSE;

   --  ------------------------------------------------------------------------

   function MSE_Prime (Y_True : Binary_Array; Y_Pred : Real_Float_List)
                       return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
      F_True : constant Real_Float_Vector := To_Real_Float_Vector (Y_True);
      F_Pred : constant Real_Float_Vector := To_Real_Float_Vector (Y_Pred);
   begin
      return 2.0 * (F_Pred - F_True) / Float (Y_True'Length);

   end MSE_Prime;

   --  --------------------------------------------------------------

end Losses;
