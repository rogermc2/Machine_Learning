
with Python;
with Support_21A;

package Python_21A is

   function Call (M : Python.Module; Function_Name : String;
                  Q : Support_21A.Float_Tensor) return Float;
end Python_21A;
