
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with NL_Types; use NL_Types;

package Fit_Functions is

   type Verbose_Type is new Integer range 0 .. 2;

   package Range_Package is new Ada.Containers.Doubly_Linked_Lists
     (float);
   subtype Range_List is Range_Package.List;

   package Param_Package is new Ada.Containers.Vectors
     (Positive, Integer);
   subtype Param_List is Param_Package.Vector;

   type Fit_Result is record
      X_Fit     : Float_List;
      Y_Fit     : Float_List;
      Params    : Param_List;
      Std_Error : Float := 0.0;
      Chi       : Float := 0.0;
   end record;

   type Func_Access is access function (Params : Param_List;
                                        X      : Float_List)
                                        return Integer_List;

   function Fit (Func           : Func_Access; X, Y : in out Float_List;
                 Default_Params : Param_List := Param_Package.Empty_Vector;
                 Data_Range     : Range_List := Range_Package.Empty_List;
                 Weighting      : Integer_List := Integer_Package.Empty_Vector;
                 Verbose        : Verbose_Type := 0;
                 Max_Iterations : Positive := 200)
                 return Fit_Result;

end Fit_Functions;
