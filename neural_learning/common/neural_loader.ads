
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types; use ML_Types;

package Neural_Loader is

   function Get_Data_Type (Data : Unbounded_String) return Data_Type;
   function Load_CSV_Data (File_Name : String) return Unbounded_List;
   function Load_CSV_Data (Data_File : File_Type) return Unbounded_List;
   function Load_Raw_CSV_Data (File_Name : String;
                               Max_Lines : Positive := 20000)
                               return Raw_Data_Vector;
   function Load_Raw_CSV_Data (Data_File : File_Type;
                               Max_Lines : Positive := 20000)
                               return Raw_Data_Vector;

   function Split_String (aString, Pattern : String) return String_List;

end Neural_Loader;
