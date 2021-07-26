
package body Fit_Functions is

   function Get_Default_Parameters (Func : Func_Access;
                                    X, Y : in out Float_List)
                                    return Param_List;

   --  -------------------------------------------------------------------------

   function Fit (Func           : Func_Access; X, Y : in out Float_List;
                 Default_Params : Param_List := Param_Package.Empty_Vector;
                 Data_Range     : Range_List := Range_Package.Empty_List;
                 Weighting      : Integer_List := Integer_Package.Empty_Vector;
                 Verbose        : Verbose_Type := 0;
                 Max_Iterations : Positive := 200)
                 return Fit_Result is
      use Ada.Containers;
      use Param_Package;
      use Range_Package;
      X_Last  : Float := X.Last_Element;
      X_Min   : Float;
      X_Max   : Float;
      X_Slice : Float_List;
      Y_Slice : Float_List;
      Beta_0  : Param_List;

      Result  : Fit_Result;
   begin
      --  If this is a histogram output correct it for the user.
      if X.Length = Y.Length + 1 then
         X.Replace_Element (X.Last_Index, X_Last/ 2.0);
      end if;

      if Data_Range /= Range_Package.Empty_List then
         X_Min := Data_Range.First_Element;
         X_Max := Data_Range.Last_Element;
         for index in X.First_Index .. X.Last_Index loop
            if X.Element (index) > X_Min and X.Element (index) < X_Max then
               X_Slice.Append (X.Element (index));
               Y_Slice.Append (Y.Element (index));
            end if;
         end loop;
         X.Clear;
         Y.Clear;
         X := X_Slice;
         Y := Y_Slice;
      end if;

      if Default_Params /= Param_Package.Empty_Vector then
         Beta_0 := Default_Params;
      else
         Beta_0 := Get_Default_Parameters (Func, X, Y);
      end if;

      return Result;
   end Fit;

   --  -------------------------------------------------------------------------
   --  Some predefined algorithms that help get the everyday functions
   --  set up for proper bounds.
   function Get_Default_Parameters (Func : Func_Access; X, Y : in out Float_List)
                                    return Param_List is
      Default_Params : Param_List;
   begin
      return Default_Params;
   end Get_Default_Parameters;

   --  ------------------------------------------------------------------------

end Fit_Functions;
