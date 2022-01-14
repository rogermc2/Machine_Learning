
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Dataset_Utilities is

   type String_Array is array (Positive range <>) of Unbounded_String;

   package Unbounded_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   subtype Unbounded_List is Unbounded_Package.Vector;

   use Unbounded_Package;
   package Raw_Data_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_List);
   subtype Raw_Data_Vector is Raw_Data_Package.Vector;

    package String_Package is new
      Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
    subtype String_List is String_Package.List;

   function Load_CSV_Data (CSV_Data : String) return String_List;
   function Split (Line : String; Sep : String) return String_Array;
   function To_Lower_Case (Text : String) return String;
   function To_Upper_Case (Text : String) return String;
   function Trimmed_Integer (Value : Integer) return String;

end Dataset_Utilities;
