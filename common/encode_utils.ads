
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Encode_Utils is

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new
     Ada.Containers.Ordered_Sets (Unbounded_String);

   Encode_Error : exception;

   function Encode (Values : Integer_Array) return Integer_Array;
   function Encode (Values        : Integer_Array; Uniques : Integer_Array;
                    Check_Unknown : Boolean := True) return Natural_Array;
   function Encode (Values : ML_Types.Value_Data_List)
                    return ML_Types.Value_Data_List;
   function Encode (Values        : ML_Types.Value_Data_List;
                    Uniques       : ML_Types.Value_Data_List;
                    Check_Unknown : Boolean := True)
                    return NL_Types.Natural_List;
   function Map_To_Integer (Values  : ML_Types.Value_Data_List;
                            Uniques : ML_Types.Value_Data_List)
                            return NL_Types.Natural_List;
   function Map_To_Integer (Values  : Integer_Array;
                            Uniques : Integer_Array) return Natural_Array;
   function Unique (Values : Binary_Matrix) return ML_Types.Integer_List;
   function Unique (Values : ML_Arrays_And_Matrices.Boolean_Array)
                    return NL_Types.Natural_List;
   function Unique (Values : Natural_Array) return Natural_Array;
   function Unique (Values : Integer_Array) return Integer_Array;
   function Unique (Values : Boolean_Array) return Boolean_Array;
   function Unique (Values : Integer_Array) return ML_Types.Integer_List;
   function Unique (List : Integer_Array_List) return ML_Types.Integer_List;
   function Unique (Values : ML_Types.Integer_List)
                    return ML_Types.Integer_List;
   function Unique (Values : ML_Types.Integer_List; Inverse : out Natural_Array)
                    return ML_Types.Integer_List;
   function Unique (Values : Integer_Array; Inverse : out Natural_Array)
                    return Integer_Array;
   function Unique (Values : ML_Arrays_And_Matrices.Integer_Array)
                    return NL_Types.Natural_List;
   function Unique (Values : Integer_Matrix) return Integer_Array;
   function Unique (Values : Integer_Matrix) return ML_Types.Integer_List;
   function Unique (Values : Integer_Matrix) return Int_Sets.Set;
   function Unique (Values : ML_Types.Array_Of_Integer_Lists)
                    return ML_Types.Integer_List;
   function Unique (Values : ML_Arrays_And_Matrices.Natural_Array)
                    return NL_Types.Natural_List;
   function Unique (Values : NL_Types.Natural_List)
                    return NL_Types.Natural_List;
   function Unique (Values : Real_Float_Matrix) return NL_Types.Float_List;
   function Unique (Values : Unbounded_String_Array)
                    return ML_Types.Unbounded_List;
   function Unique (Values : Unbounded_String_Array_List)
                    return ML_Types.Unbounded_List;
   function Unique (Values : Unbounded_String_Matrix)
                    return ML_Types.Unbounded_List;
   function Unique (Values : ML_Types.Value_Data_List)
                    return ML_Types.Value_Data_List;
   function Unique (Values  : ML_Types.Value_Data_List;
                    Inverse : out NL_Types.Natural_List)
                    return ML_Types.Value_Data_List;

end Encode_Utils;
