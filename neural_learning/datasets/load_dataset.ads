
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Load_Dataset is

   type Digits_Data_Record
     (Num_Samples, Num_Features : Positive; Num_Classes : Natural) is record
      Features  : Integer_Matrix (1 .. Num_Samples, 1 .. Num_Features);
      Target    : Integer_Array (1 .. Num_Samples);
      case Num_Classes is
         when 0 => null;
         when others =>
            Classes : Integer_Array (1 .. Num_Classes);
      end case;
   end record;

   type Iris_Data_Record is record
      Features     : NL_Types.Float_List_2D;
      Target       : NL_Types.Integer_List;
      Num_Features : Positive;

   end record;

   function Load_Digits (File_Name : String; Num_Classes : Natural := 10)
                         return Digits_Data_Record;
   function Load_Iris (File_Name : String) return Iris_Data_Record;

end Load_Dataset;
