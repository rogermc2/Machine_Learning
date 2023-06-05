
with Python;
with Python_API;

with Support_16A;

package Python_16A is

   function Call (M : Python.Module; Function_Name : String;
                  Tokeniser : Python_API.PyObject_Ptr)
                  return Support_16A.Occurrences_Dictionary;
   function Call (M : Python.Module; Function_Name : String)
                  return Support_16A.Newsgroups_Record;
end Python_16A;
