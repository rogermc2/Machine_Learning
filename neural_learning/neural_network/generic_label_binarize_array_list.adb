--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Types;

package body Generic_Label_Binarize_Array_List is

   function Find_Index (Container : Class_Array_Type;
                        Item      : Class_Type) return Natural;

   --  ------------------------------------------------------------------------

   function Binarize (Y                    : Y_Array_List_Type;
                      Classes              : Class_Array_Type;
                      Neg_Label, Pos_Label : Integer) return Binary_Matrix is
      use NL_Types.Unbounded_Package;
      Routine_Name :  constant String := "Generic_Label_Binarize.Binarize ";
      Num_Classes  : constant Positive := Classes'Length;
      Class_Index  : Natural;
      Result       : Binary_Matrix (1 .. Y'Length, 1 .. Num_Classes) :=
                       (others => (others => Neg_Label));
   begin
      for row in 1 .. Y'Length loop
         for col in 1 .. Num_Classes loop
            Class_Index :=
              Find_Index (Classes, Y (Index_Type (row), Index_Type (col)));
            Assert (Class_Index /= No_Index, Routine_Name &
                      "Binarize invalid class");
            Result (row, Class_Index) := Pos_Label;
         end loop;
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

   function Label_Binarize (Y         : Y_Array_List_Type;
                            Classes   : Class_Array_Type;
                            Neg_Label : Integer := 0;
                            Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name :  constant String :=
                       "Generic_Label_Binarize.Label_Binarize ";
      procedure Class_Sort is new
        Ada.Containers.Generic_Array_Sort (Index_Type, Class_Type,
                                           Class_Array_Type);
      Num_Classes  : constant Positive := Positive (Classes'Length);
      Y_Bin        : Binary_Matrix (1 .. Y'Length, 1 .. Num_Classes)
        := (others => (others => 0));
      Y_Kind       : Multiclass_Utils.Y_Type := Type_Of_Target (Y);
      Sorted       : Class_Array_Type (Classes'Range);

   begin
      Put_Line (Routine_Name & "Y_Kind " & Y_Type'Image (Y_Kind));
      Assert (Y_Kind /= Y_Unknown, Routine_Name &
                "unknown target data type.");
      --  L506
      Assert (Y_Kind /= Y_Continuous_Multioutput and
                Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "does not support Multioutput target data.");

      --  L516
      if Y_Kind = Y_Binary then
         declare
            Y_Bin1 : Binary_Matrix (1 .. Y'Length, 1 .. 1)
              := (others => (others => 0));
         begin
            if Num_Classes = 1 then
               for row in Y'Range loop
                  for col in Classes'First .. Classes'Last loop
                     if Neg_Label /= 0 then
                        Y_Bin1 (integer (row), 1) := Pos_Label;
                     end if;
                  end loop;
               end loop;

               return Y_Bin1;

            elsif Num_Classes > 2 then
               Y_Kind := Y_Multiclass;
            end if;
         end;
      end if;

      --  L528
      Sorted := Classes;
      Class_Sort (Sorted);

      --  L538
      if Y_Kind = Y_Binary then
         --  Label.py L539 - L549 needed to generate a csr sparse matrix
         --  Binarize is all that is needed for this implementation
         declare
            Y_Bin2 : Binary_Matrix (1 .. Y'Length, 1 .. 1)
              := (others => (others => Neg_Label));
         begin
            Y_Bin2 := Binarize (Y, Classes, Neg_Label, Pos_Label);
            return Y_Bin2;
         end;

      elsif Y_Kind = Y_Multiclass then
         declare
            Y_Bin2 : Binary_Matrix (1 .. Y'Length, 1 .. Num_Classes)
              := (others => (others => Neg_Label));
         begin
            Y_Bin2 := Binarize (Y, Classes, Neg_Label, Pos_Label);
            return Y_Bin2;
         end;

      else
         --  L551
         Assert (Y_Kind = Y_Multilabel_Indicator, Y_Type'Image (Y_Kind) &
                   " target data is not supported by " & Routine_Name);
         Y_Bin := Binarize (Y, Classes, Neg_Label, Pos_Label);
      end if;

      return Y_Bin;

   end Label_Binarize;

   --  -------------------------------------------------------------------------

end Generic_Label_Binarize_Array_List;
