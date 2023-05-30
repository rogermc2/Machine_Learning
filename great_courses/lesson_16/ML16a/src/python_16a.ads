

with Python;
with Support_16A;

package Python_16A is

   function Call_Python (M : Python.Module; Function_Name : String)
                         return Support_16A.Newsgroups_Record;
end Python_16A;
