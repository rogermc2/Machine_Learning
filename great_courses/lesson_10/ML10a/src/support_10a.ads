
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_10A is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features      : ML_Types.Value_Data_Array_2D (1 .. Num_Items,
                                               1 .. Num_Features);
      Labels        : Integer_Array (1 .. Num_Items);
      Feature_Names : Unbounded_String_Array (1 .. Num_Features);
   end record;

   function Get_Data (File_Name : String) return Data_Record;
   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float;
   pragma Inline (Error);

end Support_10A;
