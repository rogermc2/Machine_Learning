
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Strings;

with NL_Types; use NL_Types;

package Dataset_Utilities is

   type String_Array is array (Positive range <>) of Unbounded_String;
   package Unbounded_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   subtype Unbounded_List is Unbounded_Package.Vector;

   use Unbounded_Package;
   package Raw_Data_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_List);
   subtype Raw_Data_Vector is Raw_Data_Package.Vector;

   procedure CSV_Reader (CSV_File_Name : String;
                         Data          : out String_List);
   function Get_CSV_Data (CSV_Data : String) return Indef_String_List;;
   function Split (Line : String; Sep : String) return String_Array;
   function Split (Line : String; Sep : String) return
     GNATCOLL.Strings.XString_Array;
   function Split_String (aString, Pattern : String) return String_List;
   function Split_String (aString, Pattern : String)
                          return Indef_String_List;
   function To_Lower_Case (Text : String) return String;
   function To_Upper_Case (Text : String) return String;
   function To_Upper_Case (Text : Unbounded_String) return Unbounded_String;
   function Trimmed_Integer (Value : Integer) return String;

end Dataset_Utilities;
