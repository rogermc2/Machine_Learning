
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Python;
with Python_API;
with Support_Do_Why; use Support_Do_Why;

package Python_Do_Why is

   function Set_Model (Classifier : Python.Module; Data : Data_Record;
                       X_String   : Unbounded_String)
                       return Python_API.PyObject_Ptr;

end Python_Do_Why;
