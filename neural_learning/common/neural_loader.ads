
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types; use ML_Types;

package Neural_Loader is

   type Integer_Pair is record
      Integer_1 : Integer;
      Integer_2 : Integer;
   end record;

   package Integer_Pair_Package is new
     Ada.Containers.Vectors (Positive, Integer_Pair);
   subtype Integer_Pair_List is Integer_Pair_Package.Vector;

   type Float_Pair is record
      Float_1 : Float;
      Float_2 : Float;
   end record;

   package Float_Pair_Package is new
     Ada.Containers.Vectors (Positive, Float_Pair);
   subtype Float_Pair_List is Float_Pair_Package.Vector;

   function Load_CSV_Data (File_Name : String) return Unbounded_List;
   function Load_CSV_Data (Data_File : File_Type) return Unbounded_List;
   function Load_Raw_CSV_Data (File_Name : String;
                               Max_Lines : Positive := 20000)
                               return Raw_Data_Vector;
   function Load_Raw_CSV_Data (Data_File : File_Type;
                               Max_Lines : Positive := 20000)
                               return Raw_Data_Vector;

end Neural_Loader;
