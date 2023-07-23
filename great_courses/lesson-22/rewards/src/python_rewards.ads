
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Python;
with Python_API;
with Support_Rewards; use Support_Rewards;

package Python_Rewards is

   function Set_Model (Classifier : Python.Module; Data : Data_Record;
                       X_String   : Unbounded_String)
                       return Python_API.PyObject_Ptr;

end Python_Rewards;
