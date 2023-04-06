
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;

package Support_11QS is

   function Try_Clusterer (Classifier : Python.Module; Num_Clusters : Positive;
                           Clusterer  : Python_API.PyObject;
                           Train_X    : Real_Float_Matrix) return Float;
end Support_11QS;
