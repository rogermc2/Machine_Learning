
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_8A is

   type Data_Record is record
      Features : Real_Float_List;
      Labels   : ML_Types.Integer_List;
   end record;

--     function Fit (Data : Real_Float_Matrix) return Real_Float_Vector;
   function Load_Data (File_Name : String) return Data_Record;

end Support_8A;