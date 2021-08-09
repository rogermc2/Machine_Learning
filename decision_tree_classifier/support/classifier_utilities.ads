
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with Classifier_Types; use Classifier_Types;
with ML_Types;
with Weights;

package Classifier_Utilities is

   Value_Error : Exception;

   procedure Clear (anArray : in out ML_Types.Value_Data_Array);
   function Get_Column (Data       : ML_Types.List_Of_Value_Data_Lists;
                        Data_Index : Positive) return ML_Types.Value_Data_List;
   procedure Print_Integer_Array (Name : String; anArray : Integer_Array);
   procedure Print_Float_Array (Name          : String; anArray : Float_Array;
                                Start, Finish : Integer);
   procedure Print_Float_List (Name  : String; theList : Float_List);
   function Search_Sorted_Value_List (List_A, List_B : ML_Types.Value_Data_List)
                                       return Integer_List;
   function To_Float_List (A : Float_Array) return Float_List;
   function To_Array (L : Integer_List) return Integer_Array;
   function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                  return Integer_Array;
   function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array;
   --  As Integer_List, indices are part of the returned list
   function Unique (Nums : Integer_List) return Integer_List;
   function Unique_Values (Values : ML_Types.Value_Data_List)
                           return ML_Types.Value_Data_List;

end Classifier_Utilities;
