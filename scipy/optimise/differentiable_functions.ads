--  Based on scipy/optimize/_differentiable_functions.py

with System.Tasking;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;

package Differentiable_Functions is

   type Scalar_Function (X_Size : Positive) is record
      fun : System.Tasking.Access_Address;
      X   : Fortran_DP_Array (1 .. X_Size);
   end record;

end Differentiable_Functions;
