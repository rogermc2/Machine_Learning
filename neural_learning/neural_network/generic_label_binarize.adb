
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Types;

package body Generic_Label_Binarize is

   function Label_Binarize_G (Y         : Y_Array_Type;
                              Classes   : Class_Array_Type;
                              Neg_Label : Integer := 0;
                              Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name :  constant String :=
                       "Label.Label_Binarize Unbounded_String_Array ";
      procedure Class_Sort is new
        Ada.Containers.Generic_Constrained_Array_Sort (Index_Type, Class_Type,
                                                       Class_Array_Type);
      Num_Classes  : constant Positive := Positive (Classes'Length);
      Y_Bin        : Binary_Matrix (1 .. Y'Length, 1 .. Num_Classes)
        := (others => (others => 0));
      Y_Kind       : Multiclass_Utils.Y_Type := Type_Of_Target (Y);
      Sorted       : Class_Array_Type;

      function Find_Index_G (Container : Class_Array_Type;
                             Item      : Class_Type) return Natural is
         Result : Natural := 0;
      begin
         for index in Container'Range loop
            if Container (index) = Item then
               Result := Natural (index);
            end if;
         end loop;

         return Result;

      end Find_Index_G;

      function Binarize (Y_In : Y_Array_Type)
                         return Binary_Matrix is
         use NL_Types.Unbounded_Package;
         Class_Index   : Natural;
         Class_Index_1 : Natural;
         One_Class     : Boolean := True;
         Result        : Binary_Matrix (1 .. Y_In'Length, 1 .. 1) :=
                           (others => (others => Neg_Label));
      begin
         for row in 1 .. Y_In'Length loop
            Class_Index := Find_Index_G (Classes, Y_In (Index_Type (row)));
            if row = Integer (Y_In'First) then
               Class_Index_1 :=  Class_Index;
            elsif One_Class then
               One_Class := Class_Index = Class_Index_1;
            end if;

            Assert (Class_Index /= No_Index, Routine_Name &
                      "Binarize invalid class");
            if not One_Class and then Class_Index = Natural (Classes'Last) then
               Result (row, 1) := Pos_Label;
            end if;
         end loop;

         --  one class case defaults to negative label
         if One_Class then
            Result := Zero_Matrix (Result'Length, Result'Length (2));
         end if;

         return Result;

      end Binarize;

   begin
      Assert (Y_Kind /= Y_Unknown, Routine_Name &
                "unknown target data type.");
      --  L506
      Assert (Y_Kind /= Y_Continuous_Multioutput and
                Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "does not support Multioutput target data.");

      --  L516
      if Y_Kind = Y_Binary then
         Put_Line (Routine_Name & "L516 Y_Binary");
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
      Put_Line (Routine_Name & "L528");

      --  L528
      Sorted := Classes;
      Class_Sort (Sorted);
      --  L538
      if Y_Kind = Y_Binary then
         Put_Line (Routine_Name & "L538 Y_Binary");
         --  Label.py L539 - L549 needed to generate a csr sparse matrix
         --  Binarize is all that is needed for this implementation
         declare
            Y_Bin2 : Binary_Matrix (1 .. Y'Length, 1 .. 1)
              := (others => (others => Neg_Label));
         begin
            Y_Bin2 := Binarize (Y);
            return Y_Bin2;
         end;

      elsif Y_Kind = Y_Multiclass then
         Put_Line (Routine_Name & "L538 Y_Multiclass");
         declare
            Y_Bin2 : Binary_Matrix (1 .. Y'Length, 1 .. Num_Classes)
              := (others => (others => Neg_Label));
         begin
            Y_Bin2 := Binarize (Y);
            return Y_Bin2;
         end;

      else
         Put_Line (Routine_Name & "L551");
         --  L551
         Assert (Y_Kind = Y_Multilabel_Indicator, Routine_Name &
                   Y_Type'Image (Y_Kind) &
                   " target data is not supported by Label_Binarize");
         Y_Bin := Binarize (Y);
      end if;

      return Y_Bin;

   end Label_Binarize_G;

   --  -------------------------------------------------------------------------

end Generic_Label_Binarize;
