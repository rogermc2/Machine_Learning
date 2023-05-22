
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;

package Support_13QS is

   function Arg_Max (Values : Real_Float_Vector) return Integer;
   function Max (Values : Real_Float_Vector) return Float;
   function Step (M          : Python.Module; Function_Name : String;
                  Env        : Python_API.PyObject; Action : Integer;
                  Next_State : out Integer_Array; Reward : out Integer)
                  return Boolean;

end Support_13QS;
