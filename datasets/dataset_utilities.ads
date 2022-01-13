
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Dataset_Utilities is

   type String_Array is array (Positive range <>) of Unbounded_String;

   function Split (Line : String; Sep : String) return String_Array;
   function To_Lower_Case (Text : String) return String;
   function To_Upper_Case (Text : String) return String;
   function Trimmed_Integer (Value : Integer) return String;

end Dataset_Utilities;
