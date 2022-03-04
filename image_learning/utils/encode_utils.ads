
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with IL_Types; use IL_Types;

package Encode_Utils is

    package UB_String_Sets is new
      Ada.Containers.Ordered_Sets (Unbounded_String);
    Encode_Error : exception;

    function Encode (Values : Value_Data_List)
                     return Value_Data_List;
    function Encode (Values        : Value_Data_List;
                     Uniques       : Value_Data_List;
                     Check_Unknown : Boolean := True)
                     return Natural_List;
    function Map_To_Integer (Values  : Value_Data_List;
                             Uniques : Value_Data_List)
                             return Natural_List;
    function Unique (Values : Natural_List) return Natural_List;
    function Unique (Values : Value_Data_List)
                     return Value_Data_List;
    function Unique (Values  : Value_Data_List;
                     Inverse : out Natural_List)
                     return Value_Data_List;

end Encode_Utils;
