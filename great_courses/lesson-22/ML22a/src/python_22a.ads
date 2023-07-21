
with Python;
with Python_API;
with Support_22A;

package Python_22A is

   function Set_Model (Classifier : Python.Module;  Data : Support_22A.Data_Record)
                        return Python_API.PyObject_Ptr;

end Python_22A;
