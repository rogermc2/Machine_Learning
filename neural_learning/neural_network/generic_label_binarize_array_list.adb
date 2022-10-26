--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Types;

package body Generic_Label_Binarize_Array_List is

   function Find_Index (Container : Class_Array_Type;
                        Item      : Class_Type) return Natural;

   --  ------------------------------------------------------------------------

   function Binarize (Y                    : Array_List_Type;
                      Classes              : Class_Array_Type;
                      Neg_Label, Pos_Label : Integer) return Binary_Matrix is
      use NL_Types.Unbounded_Package;
      Routine_Name :  constant String := "Generic_Label_Binarize.Binarize ";
      Num_Classes  : constant Positive := Classes'Length;
      Y_Length     : constant Positive := Positive (Y.Length);
      Class_Index  : Natural;
      Result       : Binary_Matrix (1 .. Y_Length, 1 .. Num_Classes) :=
                       (others => (others => Neg_Label));
   begin
      for row in 1 .. Y_Length loop
         declare
            Y_Array : constant Y_Array_Type := Y (row);
         begin
            for col in Y_Array'First .. Y_Array'Last loop
               Class_Index := Find_Index (Classes, Y_Array (col));
               Assert (Class_Index /= No_Index, Routine_Name &
                         "Binarize invalid class");
               Result (row, Class_Index) := Pos_Label;
            end loop;
         end;
      end loop;

      return Result;

   end Binarize;

   --  ------------------------------------------------------------------------

   function Find_Index (Container : Class_Array_Type;
                        Item      : Class_Type) return Natural is
      Result : Natural := 0;
   begin
      for index in Container'Range loop
         if Container (index) = Item then
            Result := Natural (index);
         end if;
      end loop;

      return Result;

   end Find_Index;

   --  ------------------------------------------------------------------------

   function Label_Binarize (Y         : Array_List_Type;
                            Classes   : Class_Array_Type;
                            Neg_Label : Integer := 0;
                            Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name :  constant String :=
                       "Generic_Label_Binarize.Label_Binarize ";
      Y_Length     : constant Positive := Positive (Y.Length);

      procedure Class_Sort is new
        Ada.Containers.Generic_Array_Sort (Index_Type, Class_Type,
                                           Class_Array_Type);
      Num_Classes  : constant Positive := Positive (Classes'Length);
      Y_Bin        : Binary_Matrix (1 .. Y_Length, 1 .. Num_Classes)
        := (others => (others => 0));
      Sorted       : Class_Array_Type (Classes'Range);

   begin
      for row in Y.First_Index .. Y.Last_Index loop
         declare
            Y_Array : constant Y_Array_Type := Y (row);
            Y_Kind  : constant Multiclass_Utils.Y_Type := Type_Of_Target (Y_Array);
         begin
            Put_Line (Routine_Name & "Y_Kind " & Y_Type'Image (Y_Kind));
            Assert (Y_Kind /= Y_Unknown, Routine_Name &
                      "unknown target data type.");
            --  L506
            Assert (Y_Kind /= Y_Continuous_Multioutput and
                      Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                      "does not support Multioutput target data.");

            --  L516
            Assert (Y_Kind /= Y_Binary, Routine_Name & "Y_Binary not coded");

            --  L528
            Sorted := Classes;
            Class_Sort (Sorted);

            --  L538
            Assert (Y_Kind /= Y_Multiclass, Routine_Name &
                      "Y_Multiclass not coded");

            --  L551
            Assert (Y_Kind = Y_Multilabel_Indicator, Y_Type'Image (Y_Kind) &
                      " target data is not supported by " & Routine_Name);
            Y_Bin := Binarize (Y, Classes, Neg_Label, Pos_Label);
         end;
      end loop;

      return Y_Bin;

   end Label_Binarize;

   --  -------------------------------------------------------------------------

end Generic_Label_Binarize_Array_List;
