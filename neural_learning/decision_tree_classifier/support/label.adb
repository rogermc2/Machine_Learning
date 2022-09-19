
--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

--  A LabelEncoder encodes labels with a value between 0 and n_classes-1 where
--  n is the number of distinct labels. If a label repeats it assigns the same
--  value to as assigned earlier. The categorical values have been converted
--   into numeric values.
--    Examples
--      --------
--      `LabelEncoder` can be used to normalize labels:
--      >>> from sklearn import preprocessing
--      >>> le = preprocessing.LabelEncoder()
--      >>> le.fit([1, 2, 2, 6])
--      LabelEncoder()
--      >>> le.classes_
--      array([1, 2, 6])
--      >>> le.transform([1, 1, 2, 6])
--      array([0, 0, 1, 2]...)
--      >>> le.inverse_transform([0, 0, 1, 2])
--      array([1, 1, 2, 6])

--      It can also be used to transform non-numerical labels (as long as they are
--      hashable and comparable) to numerical labels.
--      >>> le = preprocessing.LabelEncoder()
--      >>> le.fit(["paris", "paris", "tokyo", "amsterdam"])
--      LabelEncoder()
--      >>> list(le.classes_)
--      ['amsterdam', 'paris', 'tokyo']
--      >>> le.transform(["tokyo", "tokyo", "paris"])
--      array([2, 2, 1]...)
--      >>> list(le.inverse_transform([2, 2, 1]))
--      ['tokyo', 'tokyo', 'paris']

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;
with Generic_Label_Binarize_Array;
with Generic_Label_Binarize_Array_List;
with Generic_Label_Binarize_Matrix;
--  with Printing;
--  with Test_Support;

package body Label is

   function Transform_CM
     (Y : Integer_Array_List; Class_Mapping : in out Integer_List)
      return Binary_Matrix;
   function Transform_CM
     (Y : Integer_Matrix; Class_Mapping : in out Integer_List)
      return Binary_Matrix;
   function Transform_CM
     (Y             : Unbounded_String_Array_List;
      Class_Mapping : in out Unbounded_List) return Binary_Matrix;

   --  -------------------------------------------------------------------------

   procedure C_Init (LB        : in out Label_Binarizer;
                     Neg_Label : Float := 0.0;
                     Pos_Label : Float := 1.0) is
   begin
      LB.Neg_Label := Neg_Label;
      LB.Pos_Label := Pos_Label;

   end C_Init;

   --  -------------------------------------------------------------------------

   procedure C_Init (MLB     : in out Multi_Label_Binarizer;
                     Classes : Integer_List := Integer_Package.Empty_Vector) is
   begin
      MLB.Classes := Classes;

   end C_Init;

   --  -------------------------------------------------------------------------
   --  L742
   procedure Fit (Binarizer : in out Label_Binarizer; Classes : Integer_List) is
      use Integer_Sorting;
      Routine_Name : constant String := "Label.Binarizer Fit Classes ";
      L_Classes    : Integer_List := Classes;
   begin
      if Binarizer.Classes.Is_Empty then
         Sort (L_Classes);
         Binarizer.Classes := L_Classes;
      else
         for index in Binarizer.Classes.First_Index ..
           Binarizer.Classes.Last_Index - 1 loop
            for index_2 in index + 1 .. Binarizer.Classes.Last_Index loop
               Assert (Binarizer.Classes (index_2) /= Binarizer.Classes (index),
                       Routine_Name &
                         "Binarizer.Classes contains duplicate classes");
            end loop;
         end loop;
      end if;

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Array) is
      use Multiclass_Utils;
      Routine_Name : constant String := "Label.Binarizer Fit ";
   begin
      Assert (Binarizer.Y_Kind /= Y_Continuous_Multioutput and
                Binarizer.Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "label binarization does not support multioutput target data");
      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

   end Fit;

   --  -------------------------------------------------------------------------
   --  L264
   procedure Fit (Binarizer : in out Label_Binarizer; Y : Binary_Matrix) is
      use Multiclass_Utils;
      Routine_Name : constant String := "Label.Fit Binarizer Binary_Matrix ";
   begin
      Assert (Binarizer.Neg_Label < Binarizer.Pos_Label, Routine_Name &
                "Neg_Label " & Float'Image (Binarizer.Neg_Label)  &
                " must be less than Pos_Label " &
                Float'Image (Binarizer.Pos_Label));
      Assert (Binarizer.Y_Kind /= Y_Continuous_Multioutput and
                Binarizer.Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "label binarization does not support multioutput target data");

      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

   end Fit;

   --  -------------------------------------------------------------------------
   --  L264
   procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Matrix) is
      use Multiclass_Utils;
      Routine_Name : constant String := "Label.Fit Binarizer Integer_Matrix ";
   begin
      Assert (Binarizer.Y_Kind /= Y_Continuous_Multioutput and
                Binarizer.Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "label binarization does not support multioutput target data");
      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Fit (Binarizer : in out Label_Binarizer;
                  Y         : NL_Types.Array_Of_Integer_Lists) is
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Fit Binarizer Array_Of_Integer_Lists ";
   begin
      Assert (Binarizer.Y_Kind /= Y_Continuous_Multioutput and
                Binarizer.Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "label binarization does not support multioutput target data");
      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Fit (Binarizer : in out UB_Label_Binarizer;
                  Y         : Unbounded_String_Array) is
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Fit Binarizer Unbounded_String_Array ";
   begin
      Assert (Binarizer.Y_Kind /= Y_Continuous_Multioutput and
                Binarizer.Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "label binarization does not support multioutput target data");
      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Fit (Binarizer : in out UB_Label_Binarizer;
                  Y         : Unbounded_String_Matrix) is
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Fit Binarizer Unbounded_String_Array ";
   begin
      Assert (Binarizer.Y_Kind /= Y_Continuous_Multioutput and
                Binarizer.Y_Kind /= Y_Multiclass_Multioutput, Routine_Name &
                "label binarization does not support multioutput target data");
      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);

   end Fit;

   --  -------------------------------------------------------------------------
   --  L740 Fit
   procedure Fit (Binarizer : in out Multi_Label_Binarizer;
                  Y         : Integer_Array_List) is
      Routine_Name : constant String :=
                       "Label.Fit MLB Integer_Array_List ";
      Classes      : NL_Types.Integer_List;
      Duplicates   : Boolean := False;
   begin
      if Binarizer.Classes.Is_Empty then
         --  L758
         Classes := Multiclass_Utils.Unique_Labels (Y);
      else
         --  L759
         for index in Classes.First_Index .. Classes.Last_Index - 1 loop
            Duplicates := Duplicates and Classes (index) in
              Classes.First_Index + index - 1 .. Classes.Last_Index;
            Assert (not Duplicates, Routine_Name &
                      "Classes contains duplicates.");
         end loop;
         Classes := Binarizer.Classes;
      end if;

      Binarizer.Classes := Classes;

   end Fit;

   --  -------------------------------------------------------------------------
   --  L740 Fit
   procedure Fit (Binarizer : in out Multi_Label_Binarizer;
                  Y         : Integer_Matrix) is
      Routine_Name : constant String :=
                       "Label.Fit Multi_Label_Binarizer ";
      Classes      : NL_Types.Integer_List;
      Duplicates   : Boolean := False;
   begin
      if Binarizer.Classes.Is_Empty then
         --  L758
         Classes := Multiclass_Utils.Unique_Labels (Y);
      else
         --  L759
         for index in Classes.First_Index .. Classes.Last_Index - 1 loop
            Duplicates := Duplicates and Classes (index) in
              Classes.First_Index + index - 1 .. Classes.Last_Index;
            Assert (not Duplicates, Routine_Name &
                      "Classes contains duplicates.");
         end loop;
         Classes := Binarizer.Classes;
      end if;

      Binarizer.Classes := Classes;

   end Fit;

   --  -------------------------------------------------------------------------
   --  L740 Fit
   procedure Fit (Binarizer : in out UB_Multi_Label_Binarizer;
                  Y         : Unbounded_String_Array_List) is
      --        Routine_Name : constant String :=
      --                         "Label.Fit MLB Unbounded_String_Array_List ";
      Classes      : NL_Types.Unbounded_List;
      --        Duplicates   : Boolean := False;
   begin
      if Binarizer.Classes.Is_Empty then
         --  L758
         Classes := Multiclass_Utils.Unique_Labels (Y);
      else
         --  L759
         --           for index in Classes.First_Index .. Classes.Last_Index - 1 loop
         --              Duplicates := Duplicates and Classes (index) in
         --                Classes.First_Index + index - 1 .. Classes.Last_Index;
         --              Assert (not Duplicates, Routine_Name &
         --                        "Classes contains duplicates.");
         --           end loop;
         Classes := Binarizer.Classes;
      end if;

      Binarizer.Classes := Classes;

   end Fit;

   --  -------------------------------------------------------------------------

   procedure Fit (Encoder : in out Label_Encoder; Y : Integer_Array) is
      Routine_Name : constant String := "Label.Fit Encoder ";
   begin
      Assert (Encoder.Encoder_Kind = Class_Unique, Routine_Name &
                "Label.Fit called with label encoder instead of a"
              & " unique encode");
      Encoder.Uniques := Encode_Utils.Unique (Y);
   end Fit;

   --  -------------------------------------------------------------------------
   --  L305
   function Fit_Transform (Binarizer : in out Label_Binarizer;
                           Y         : Integer_Matrix) return Binary_Matrix is
      Routine_Name  : constant String := "Label.Fit_Transform Integer_Matrix";
   begin
      Put_Line (Routine_Name);
      Fit (Binarizer, Y);
      return Transform (Binarizer, Y);

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --  L789
   function Fit_Transform (Binarizer : in out Multi_Label_Binarizer;
                           Y         : Integer_Array_List)
                           return Binary_Matrix is
      use Integer_Sorting;
      Routine_Name  : constant String :=
                        "Label.Fit_Transform Integer_Array_List ";
   begin
      Assert (not Y.Is_Empty, Routine_Name & "Y is empty");
      if not Binarizer.Classes.Is_Empty then
         Fit (Binarizer, Y);
         return Transform (Y);

      else
         declare
            Class_Mapping : Integer_List;
            --  L814 yt = self._transform(y, class_mapping)
            CM_Matrix     : constant Binary_Matrix :=
                              Transform_CM (Y, Class_Mapping);
         begin
            for key in Class_Mapping.First_Index ..
              Class_Mapping.Last_Index loop
               Binarizer.Classes.Append (Class_Mapping.Element (key));
            end loop;

            --  L817
            Sort (Binarizer.Classes);
            return CM_Matrix;
         end;
      end if;

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --  L789
   function Fit_Transform (Binarizer : in out Multi_Label_Binarizer;
                           Y         : Integer_Matrix) return Binary_Matrix is
      --        Routine_Name  : constant String :=
      --                            "Label.Fit_Transform Integer_Matrix ";
   begin
      Fit (Binarizer, Y);
      return Transform (Y);

   end Fit_Transform;

   --  -------------------------------------------------------------------------

   function Fit_Transform (Binarizer : in out UB_Label_Binarizer;
                           Y         : Unbounded_String_Array)
                           return Binary_Matrix is
      --        Routine_Name : constant String :=
      --                         "Label.Fit_Transform Unbounded_String_Array ";
   begin
      --        Test_Support.Print_Unbound_Array  (Routine_Name & "Y", Y);
      Fit (Binarizer, Y);

      return Transform (Binarizer, Y);

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --  L789
   function Fit_Transform (Binarizer : in out UB_Multi_Label_Binarizer;
                           Y         : Unbounded_String_Array_List)
                           return Binary_Matrix is
      use Unbounded_Sorting;
      Routine_Name  : constant String :=
                        "Label.Fit_Transform Unbounded_String_Array_List ";
   begin
      Assert (not Y.Is_Empty, Routine_Name & "Y is empty");
      if not Binarizer.Classes.Is_Empty then
         Fit (Binarizer, Y);
         return Transform (Y);

      else
         declare
            Class_Mapping : Unbounded_List;
            --  L814 yt = self._transform(y, class_mapping)
            CM_Matrix     : constant Binary_Matrix :=
                              Transform_CM (Y, Class_Mapping);
         begin
            for key in Class_Mapping.First_Index ..
              Class_Mapping.Last_Index loop
               Binarizer.Classes.Append (Class_Mapping.Element (key));
            end loop;

            --  L817
            Sort (Binarizer.Classes);
            return CM_Matrix;
         end;
      end if;

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --  L305
   function Fit_Transform
     (Binarizer : in out UB_Label_Binarizer; Y : Unbounded_String_Matrix)
      return Binary_Matrix is
   begin
      Put_Line ("Label.Fit_Transform Unbounded_String_Matrix");
      Fit (Binarizer, Y);
      return Transform (Binarizer, Y);

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   --  Balanced class weights should be given by
   --  n_samples / (n_classes * np.bincount(y))
   function Fit_Transform (Encoder : in out Label_Encoder; Y : Integer_Array)
                           return Natural_Array is
      Routine_Name   : constant String :=
                         "Label.Fit_Transform Encoder Integer_Array ";
      Encoded_Labels : Natural_Array (1 .. Y'Length);
   begin
      Put_Line (Routine_Name);
      if Encoder.Encoder_Kind = Class_Unique then
         Encoder.Uniques := Encode_Utils.Unique (Y, Encoded_Labels);
      else
         raise Label_Error with Routine_Name &
           "called with label encoder instead of" &
           " unique encoder";
      end if;

      return Encoded_Labels;

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --  L586
   function Inverse_Binarize_Multiclass
     (Y_Prob : Real_Float_Matrix; Classes : Integer_List)
      return Integer_Matrix is
      use Classifier_Utilities;
      --          Routine_Name   :  constant String :=
      --                             "Label.Inverse_Binarize_Multiclass Float_Matrix ";
      Inverse        : Integer_Matrix  (Y_Prob'Range, 1 .. 1);
      Max_Indices    : Integer_Array (Y_Prob'Range);
   begin
      --  L627
      Max_Indices := Max_Probability_Indices (Y_Prob);
      for row in Inverse'Range loop
         Inverse (row, 1) := Classes.Element (Max_Indices (row));
      end loop;

      return Inverse;

   end Inverse_Binarize_Multiclass;

   --  -------------------------------------------------------------------------
   --  L586
   function Inverse_Binarize_Multiclass
     (Y : Binary_Matrix; Classes : Integer_List) return Integer_Matrix is
      --        Routine_Name     :  constant String :=
      --                             "Label.Inverse_Binarize_Multiclass Integer_Matrix ";
      Dim              : Natural := 0;
   begin
      --  L627
      for col in Y'Range (2) loop
         if Y (1, col) = 1 then
            Dim := Dim + 1;
         end if;
      end loop;

      declare
         Inverse : Integer_Matrix  (Y'Range, 1 .. Dim);
         I_Col   : Natural;
      begin
         for row in Y'Range loop
            I_Col := 0;
            for col in Y'Range (2) loop
               if Y (row, col) = 1 then
                  --                    Put_Line (Routine_Name & "col" & Integer'Image (col));
                  I_Col := I_Col + 1;
                  --                    Put_Line (Routine_Name & "I_Col" & Integer'Image (I_Col));
                  Inverse (row, I_Col) := Classes (col);
               end if;
            end loop;
         end loop;

         return Inverse;
      end;

   end Inverse_Binarize_Multiclass;

   --  -------------------------------------------------------------------------
   --  L586
   function Inverse_Binarize_Multiclass
     (Y : Binary_Matrix ; Classes : Unbounded_List)
      return Unbounded_String_Array is
      use Classifier_Utilities;
      --          Routine_Name   :  constant String :=
      --                             "Label.Inverse_Binarize_Multiclass Binary_Matrix ";
      Inverse     : Unbounded_String_Array (Y'Range);
      Max_Indices : Integer_Array (Y'Range);
   begin
      --  L627
      Max_Indices := Max_Probability_Indices (Y);
      for row in Inverse'Range loop
         Inverse (row) := Classes.Element (Max_Indices (row));
      end loop;

      return Inverse;

   end Inverse_Binarize_Multiclass;

   --  -------------------------------------------------------------------------
   --  L586
   function Inverse_Binarize_Multiclass
     (Y : Binary_Matrix ; Classes : Integer_List) return Integer_Array_List is
      --          Routine_Name   :  constant String :=
      --                             "Label.Inverse_Binarize_Multiclass Integer_Array_List ";
      Inverse          : Integer_Array_List;
      Row_Classes      : Integer_List;
      Row_Classes_List : Integer_List_2D;
   begin
      --  L627
      for row in Y'Range loop
         Row_Classes.Clear;
         for col in Y'Range (2) loop
            if Y (row, col) = 1 then
               Row_Classes.Append (Classes (col));
            end if;
         end loop;
         Row_Classes_List.Append (Row_Classes);
      end loop;

      for row in Row_Classes_List.First_Index ..
        Row_Classes_List.Last_Index loop
         declare
            Row_Classes   : constant Integer_List := Row_Classes_List (row);
            Inverse_Array : Integer_Array
              (1 .. Positive (Row_Classes.Length));
         begin
            for col in Row_Classes.First_Index .. Row_Classes.Last_Index loop
               Inverse_Array (col) := Row_Classes (col);
            end loop;
            Inverse.Append (Inverse_Array);
         end;
      end loop;

      return Inverse;

   end Inverse_Binarize_Multiclass;

   --  -------------------------------------------------------------------------
   --  L586
   function Inverse_Binarize_Multiclass
     (Y : Binary_Matrix ; Classes : Unbounded_List)
      return Unbounded_String_Array_List is
      --          Routine_Name   :  constant String :=
      --                             "Label.Inverse_Binarize_Multiclass Unbounded_String_Array_List ";
      Inverse          : Unbounded_String_Array_List;
      Row_Classes      : Unbounded_List;
      Row_Classes_List : Unbounded_List_2D;
   begin
      --  L627
      for row in Y'Range loop
         Row_Classes.Clear;
         for col in Y'Range (2) loop
            if Y (row, col) = 1 then
               Row_Classes.Append (Classes (col));
            end if;
         end loop;
         Row_Classes_List.Append (Row_Classes);
      end loop;

      for row in Row_Classes_List.First_Index ..
        Row_Classes_List.Last_Index loop
         declare
            Row_Classes   : constant Unbounded_List := Row_Classes_List (row);
            Inverse_Array : Unbounded_String_Array
              (1 .. Positive (Row_Classes.Length));
         begin
            for col in Row_Classes.First_Index .. Row_Classes.Last_Index loop
               Inverse_Array (col) := Row_Classes (col);
            end loop;
            Inverse.Append (Inverse_Array);
         end;
      end loop;

      return Inverse;

   end Inverse_Binarize_Multiclass;

   --  -------------------------------------------------------------------------

   --  L586
   function Inverse_Binarize_Multiclass
     (Y_Prob : Binary_Matrix ; Classes : Unbounded_List)
      return Unbounded_String_Matrix is
      use Classifier_Utilities;
      --          Routine_Name   :  constant String :=
      --                             "Label.Inverse_Binarize_Multiclass Binary_Matrix ";
      Inverse     : Unbounded_String_Matrix (Y_Prob'Range, Y_Prob'Range (2));
      Max_Indices : Integer_Array (Y_Prob'Range);
   begin
      --  L627
      Max_Indices := Max_Probability_Indices (Y_Prob);
      for row in Inverse'Range loop
         for col in Inverse'Range (2) loop
            Inverse (row, col) := Classes.Element (Max_Indices (row));
         end loop;
      end loop;

      return Inverse;

   end Inverse_Binarize_Multiclass;

   --  -------------------------------------------------------------------------

   function Inverse_Binarize_Thresholding
     (Y       : Real_Float_Matrix; Output_Type : Multiclass_Utils.Y_Type;
      Classes : Integer_List; Threshold : Float)
      return Integer_Matrix is
      use Ada.Containers;
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Inverse_Binarize_Thresholding ";
      Num_Classes  : constant Positive := Positive (Classes.Length);
      Y_Thresh     : Integer_Matrix (Y'Range, Y'Range (2)) :=
                       (others => (others => 0));
   begin
      --        Test_Support.Print_Matrix_Dimensions (Routine_Name & "Y", Y);
      --        Put_Line (Routine_Name & "Num_Classes:" & Integer'Image (Num_Classes));
      if Output_Type = Y_Binary then
         Assert (Y'Length (2) <= 2, Routine_Name &
                   "output_type is binary but Y'Length (2) is " &
                   Integer'Image (Y'Length (2)));
      else
         Assert (Y'Length (2) /= Num_Classes, Routine_Name &
                   "output_type is binary but Y'Length (2) " &
                   Integer'Image (Y'Length (2)) & " is not Num_Classes" &
                   Integer'Image (Num_Classes));
      end if;

      --  L653
      for row in Y_Thresh'Range loop
         for col in Y_Thresh'Range (2) loop
            if Y (row, col) > Threshold then
               Y_Thresh (row, col) := 1;
            end if;
         end loop;
      end loop;

      --  L657
      if Output_Type = Y_Binary then
         --           Put_Line (Routine_Name & "Output_Type = Y_Binary");
         declare
            Inverse      : Integer_Matrix (Y'Range, 1 .. 1) :=
                             (others => (others => 0));
         begin
            if Y_Thresh'Length (2) = 2 then
               --              Put_Line (Routine_Name & "Thresh'Length (2) = 2");
               for row in Y_Thresh'Range loop
                  for col in Y_Thresh'Range (2) loop
                     Inverse (row, 1) := Classes (Y_Thresh (row, 2) + 1);
                  end loop;
               end loop;

            elsif Classes.Length = 1 then
               Assert (False, Routine_Name &
                         "Y_Binary Classes.Length = 1 not coded");
            else
               Assert (False, Routine_Name &
                         "Y_Binary Classes.Length /= 1 not coded");
            end if;
            return Inverse;
         end;

      elsif Output_Type = Y_Multilabel_Indicator then
         return Y_Thresh;
      else
         Assert (False, Routine_Name & Y_Type'Image (Output_Type) &
                   " format is not supported");
         return Y_Thresh;
      end if;

   end Inverse_Binarize_Thresholding;

   --  -------------------------------------------------------------------------

   --      function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix)
   --                                 return Real_Float_Matrix is
   --          use Multiclass_Utils;
   --          Y_Inv     : Real_Float_Matrix (1 .. Y'Length (2), 1 .. Y'Length);
   --          --        Threshold : Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
   --      begin
   --          if Self.Y_Kind = Y_Multiclass then
   --              Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
   --          else
   --              Assert (False, "Label.Inverse_Transform Boolean_Matrix not Y_Multiclass" &
   --                        "not coded");
   --              --   Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
   --          end if;
   --
   --          return Y_Inv;

   --      end Inverse_Transform;

   --  -------------------------------------------------------------------------

   --     function Inverse_Transform (Self : Label_Binarizer; Y : Float_Matrix)
   --                                  return Float_Matrix is
   --        use Multiclass_Utils;
   --        Y_Inv     : Float_Matrix (1 .. Y'Length, 1 .. 1);
   --        Threshold : Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
   --     begin
   --        if Self.Y_Kind = Y_Multiclass then
   --           Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
   --        else
   --           null;
   --           --   Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
   --        end if;
   --
   --        return Y_Inv;
   --
   --     end Inverse_Transform;

   --  -------------------------------------------------------------------------

   --      function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix)
   --                                 return Integer_Matrix is
   --          use Multiclass_Utils;
   --          Y_Inv     : Integer_Matrix (1 .. Y'Length (2), 1 .. Y'Length);
   --          --        Threshold : Float := (Self.Pos_Label + Self.Neg_Label) / 2.0;
   --      begin
   --          if Self.Y_Kind = Y_Multiclass then
   --              Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
   --          else
   --              Assert (False, "Label.Inverse_Transform Boolean_Matrix return " &
   --                        "Integer_Matrix not Y_Multiclass not coded");
   --              --   Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Threshold);
   --          end if;
   --
   --          return Y_Inv;
   --
   --      end Inverse_Transform;

   --  -------------------------------------------------------------------------

   --   Inverse_Transform transforms class labels back to original encoding
   function Inverse_Transform (Self : Label_Encoder; Labels : Natural_Array)
                               return Integer_Array is
      --        Routine_Name :  constant String := "Label.Inverse_Transform ";
      aRange       : Integer_Array (1 .. Positive (Self.Uniques'Length));
      Diff         : Natural_List;
      Result       : Integer_Array (1 .. Positive (Labels'Length));
   begin
      for index in aRange'Range loop
         aRange (index) := index;
      end loop;

      Diff := Classifier_Utilities.Set_Diff (Labels, aRange);
      Assert (Diff.Is_Empty,
              "Label.Inverse_Transform Labels vector contains " &
                "previously unseen labels.");

      for index in Result'Range loop
         Result (index) := Self.Uniques (Labels (index));
      end loop;

      return Result;

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : Label_Encoder; Y : Integer_Array)
                               return Integer_Array is
      aRange  : Integer_Array (1 .. Positive (Self.Uniques'Length));
      Diff    : Natural_List;
      Result  : Integer_Array (1 .. Positive (Y'Length));
   begin
      for index in Self.Classes'Range loop
         aRange (index) := index;
      end loop;

      Diff := Classifier_Utilities.Set_Diff (Y, aRange);
      Assert (Diff.Is_Empty, "Y contains previously unseen labels.");

      for index in Y'Range loop
         Result (index) := Self.Classes (Y (index));
      end loop;

      return Result;

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : Label_Encoder; Y : Integer_Matrix)
                               return Integer_Matrix is
      YT        : constant Integer_Matrix := Transpose (Y);
      aRange    : Integer_Array (1 .. Y'Length);
      Diff      : Natural_List;
      Transform : Integer_Matrix (1 .. YT'Length, 1 .. YT'Length (2));
      YT_Row    : Integer_Array (1 .. YT'Length);
   begin
      for r in Self.Classes'Range loop
         aRange (r) := r;
      end loop;

      for Y_index in YT'Range loop
         for index2 in YT'Range (2) loop
            YT_Row (Y_index) := YT (Y_index, index2);
         end loop;
         Diff := Classifier_Utilities.Set_Diff (YT_Row, aRange);
         Assert (Diff.Is_Empty, "Y contains previously unseen labels.");

         for index2 in YT_Row'Range loop
            Transform (Y_index, index2) := Self.Classes (YT_Row (index2));
         end loop;
      end loop;

      return Transpose (Transform);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------
   --  L361
   function Inverse_Transform (Self : Label_Binarizer; Y : Real_Float_Matrix)
                               return Integer_Matrix is
      use Multiclass_Utils;
      --              Routine_Name : constant String := "Label.Inverse_Transform ";
      Threshold    : constant Float := (Self.Pos_Label + Self.Neg_Label)
                       / 2.0;
   begin
      --        Test_Support.Print_Matrix_Dimensions (Routine_Name & "Y", Y);
      --  L398
      if Self.Y_Kind = Y_Multiclass then
         return Inverse_Binarize_Multiclass (Y, Self.Classes);
      else
         return Inverse_Binarize_Thresholding
           (Y, Self.Y_Kind, Self.Classes, Threshold);
      end if;

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : Multi_Label_Binarizer; Y : Binary_Matrix)
                               return Integer_Array_List is
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Inverse_Transform MLB Integer_Array_List ";
   begin
      --  L398
      Assert (Self.Y_Kind = Y_Multiclass, Routine_Name &
                "only supports Y_Multiclass");
      return Inverse_Binarize_Multiclass (Y, Self.Classes);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : UB_Multi_Label_Binarizer; Y : Binary_Matrix)
                               return Unbounded_String_Array_List is
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Inverse_Transform MLB Unbounded_String_Array_List ";
   begin
      --  L398
      Assert (Self.Y_Kind = Y_Multiclass, Routine_Name &
                "only supports Y_Multiclass");
      return Inverse_Binarize_Multiclass (Y, Self.Classes);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : Multi_Label_Binarizer; Y : Binary_Matrix)
                               return Integer_Matrix is
      use Multiclass_Utils;
      Routine_Name : constant String :=
                       "Label.Inverse_Transform MLB Integer_Matrix ";
   begin
      --  L398
      Assert (Self.Y_Kind = Y_Multiclass, Routine_Name &
                "only supports Y_Multiclass");
      return Inverse_Binarize_Multiclass (Y, Self.Classes);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : UB_Label_Binarizer; Y : Binary_Matrix)
                               return Unbounded_String_Array is
      --        Routine_Name : constant String :=
      --          "Label.Inverse_Transform Unbounded_String_Array ";
   begin
      --  L398
      return Inverse_Binarize_Multiclass (Y, Self.Classes);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : UB_Label_Binarizer; Y : Binary_Matrix)
                               return Unbounded_String_Matrix is
      --        Routine_Name : constant String :=
      --          "Label.Inverse_Transform Unbounded_String_Matrix ";
   begin
      --  L398
      return Inverse_Binarize_Multiclass (Y, Self.Classes);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   --  L361 Inverse_Transform transforms binary labels back to
   --       multi-class labels
   --     function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix;
   --                                 Use_Threshold : Boolean := False;
   --                                 Threshold     : Float := 0.0)
   --                                  return Boolean_Matrix is
   --        Routine_Name :  constant String := "Label.Inverse_Transform Binarizer ";
   --        use Multiclass_Utils;
   --        Y_Inv  : Boolean_Matrix (Y'First (2) .. Y'Last (2), Y'First .. Y'Last);
   --        Thresh : Float;
   --     begin
   --        Put_Line (Routine_Name);
   --        if Self.Y_Kind = Y_Multiclass then
   --  --           Y_Inv := Inverse_Binarize_Multiclass (Y, Self.Classes);
   --           null;
   --        else
   --           if Use_Threshold then
   --              Thresh := Threshold;
   --           else
   --              Thresh := 0.5 * (Self.Neg_Label + Self.Pos_Label);
   --           end if;
   --
   --           Y_Inv := Inverse_Binarize_Thresholding (Y, Self.Classes, Thresh);
   --        end if;
   --
   --        return Y_Inv;
   --
   --     end Inverse_Transform;

   --  -------------------------------------------------------------------------

   --  L416
   function Label_Binarize (Y, Classes : Integer_List;
                            Neg_Label  : Integer := 0;
                            Pos_Label  : Integer := 1) return Binary_Matrix is
      Routine_Name  :  constant String := "Label.Label_Binarize Integer_List ";
      Classes_Array : constant Integer_Array := To_Integer_Array (Classes);
      Y_Array       : constant Integer_Array := To_Integer_Array (Y);
      package Label_Binarize_Int is new Generic_Label_Binarize_Array
        (Index_Type => Natural, Class_Type => Integer,
         Y_Array_Type  => Integer_Array ,
         Class_Array_Type => Integer_Array,
         Type_Of_Target   => Multiclass_Utils.Type_Of_Target, "<" => "<");
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "called with empty Classes list");
      return Label_Binarize_Int.Label_Binarize
        (Y_Array, Classes_Array, Neg_Label, Pos_Label);

   end Label_Binarize;

   --  -------------------------------------------------------------------------

   function Label_Binarize (Y         : Binary_Matrix; Classes : Integer_List;
                            Neg_Label : Integer := 0) return Binary_Matrix is
      Routine_Name :  constant String := "Label.Label_Binarize Binary_Matrix ";
      Y_Int        : Integer_Matrix (Y'Range, Y'Range (2));
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "called with empty Classes list");
      for row in Y'Range loop
         for col in Y'Range (2) loop
            Y_Int (row, col) := Y (row, col);
         end loop;
      end loop;

      return Label_Binarize (Y_Int, Classes, Neg_Label);

   end Label_Binarize;

   --  -------------------------------------------------------------------------
   --  L416
   function Label_Binarize (Y         : Integer_Array_List;
                            Classes   : Integer_List;
                            Neg_Label : Integer := 0;
                            Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name  :  constant String :=
                        "Label.Label_Binarize Integer_Array_List ";
      Classes_Array : constant Integer_Array :=
                        To_Integer_Array (Classes);
      package Label_Binarize_Integer_Array_List is new
        Generic_Label_Binarize_Array_List
          (Index_Type         => Natural,
           Class_Type         => Integer,
           Y_Array_Type       => Integer_Array,
           Array_List_Package => Integer_Array_Package,
           Class_Array_Type   => Integer_Array,
           Type_Of_Target     => Multiclass_Utils.Type_Of_Target,
           "<"                => "<");
   begin
      Put_Line (Routine_Name);
      Assert (not Classes.Is_Empty, Routine_Name &
                "called with empty Classes list");
      return Label_Binarize_Integer_Array_List.Label_Binarize
        (Y, Classes_Array, Neg_Label, Pos_Label);

   end Label_Binarize;

   --  -------------------------------------------------------------------------
   --  L416
   function Label_Binarize (Y         : Integer_Matrix;
                            Classes   : Integer_List;
                            Neg_Label : Integer := 0;
                            Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name  :  constant String := "Label.Label_Binarize Integer_Matrix ";
      Classes_Array : constant Integer_Array :=
                        To_Integer_Array (Classes);
      package Label_Binarize_Integer is new Generic_Label_Binarize_Matrix
        (Index_Type => Natural, Class_Type => Integer,
         Y_Matrix_Type => Integer_Matrix,
         Class_Array_Type => Integer_Array,
         Type_Of_Target   => Multiclass_Utils.Type_Of_Target, "<" => "<");
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "called with empty Classes list");
      return Label_Binarize_Integer.Label_Binarize
        (Y, Classes_Array, Neg_Label, Pos_Label);

   end Label_Binarize;

   --  -------------------------------------------------------------------------
   --  L416
   function Label_Binarize (Y         : Unbounded_String_Matrix;
                            Classes   : Unbounded_List;
                            Neg_Label : Integer := 0;
                            Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name  :  constant String :=
                        "Label.Label_Binarize Unbounded_String_Matrix ";
      Classes_Array : constant Unbounded_String_Array :=
                        To_Unbound_Array (Classes);
      package Label_Binarize_UB is new Generic_Label_Binarize_Matrix
        (Index_Type => Integer, Class_Type => Unbounded_String,
         Y_Matrix_Type  => Unbounded_String_Matrix ,
         Class_Array_Type => Unbounded_String_Array,
         Type_Of_Target   => Multiclass_Utils.Type_Of_Target,
         "<" => Ada.Strings.Unbounded."<");
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "called with empty Classes list");
      return Label_Binarize_UB.Label_Binarize
        (Y, Classes_Array, Neg_Label, Pos_Label);

   end Label_Binarize;

   --  -------------------------------------------------------------------------
   --  L416
   function Label_Binarize (Y         : Unbounded_String_Array;
                            Classes   : Unbounded_List;
                            Neg_Label : Integer := 0;
                            Pos_Label : Integer := 1) return Binary_Matrix is
      Routine_Name  :  constant String :=
                        "Label.Label_Binarize Unbounded_String_Array ";
      Classes_Array : constant Unbounded_String_Array :=
                        To_Unbound_Array (Classes);
      package Label_Binarize_UB is new Generic_Label_Binarize_Array
        (Index_Type => Integer, Class_Type => Unbounded_String,
         Y_Array_Type  => Unbounded_String_Array ,
         Class_Array_Type => Unbounded_String_Array,
         Type_Of_Target   => Multiclass_Utils.Type_Of_Target,
         "<" => Ada.Strings.Unbounded."<");
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "called with empty Classes list");
      return Label_Binarize_UB.Label_Binarize
        (Y, Classes_Array, Neg_Label, Pos_Label);

   end Label_Binarize;

   --  -------------------------------------------------------------------------

   function Transform (Self : Label_Binarizer; Y : Binary_Matrix)
                       return Binary_Matrix is
      --        Routine_Name : constant String := "Label.Transform Binarize Binary Y ";
   begin
      return Label_Binarize (Y, Self.Classes);

   end Transform;

   --  -------------------------------------------------------------------------
   --  L327  Transform transforms multi-class labels to binary labels.
   function Transform (Self : Label_Binarizer; Y : Integer_Matrix)
                       return Binary_Matrix is
      --        Routine_Name : constant String := "Label.Transform Binarize Integer Y ";
   begin
      return Label_Binarize (Y, Self.Classes);

   end Transform;

   --  -------------------------------------------------------------------------

   function Transform (Self : Label_Binarizer; Y : Integer_List)
                       return Binary_Matrix is
      --  Routine_Name : constant String := "Label.Transform Binarize ";
   begin

      return Label_Binarize (Y, Self.Classes);

   end Transform;

   --  -------------------------------------------------------------------------

   function Transform (Self : Label_Binarizer; Y : Array_Of_Integer_Lists)
                       return Binary_Matrix is
      use Integer_Package;
      Routine_Name  : constant String := "Label.Transform Array_Of_Integer_Lists ";
      Y_Row         : Integer_List;
      Classes       : Integer_List;
      Indices       : Integer_List_2D;
      Class_Curs    : Cursor;
      Classes_Array : Array_Of_Integer_Lists (1 .. Y'Length);
   begin
      for row in Y'Range loop
         Y_Row := Y (row);
         Classes.Clear;
         for col in Y_Row.First_Index .. Y_Row.Last_Index loop
            Class_Curs := Self.Classes.Find (Y_Row (col));
            if Class_Curs /= No_Element then
               Classes.Append (Element (Class_Curs));
            else
               Assert (False, Routine_Name & "Class not found");
            end if;
         end loop;

         Indices.Append (Classes);
         Classes_Array (row) := Classes;
      end loop;

      --  Binarize
      declare
         Class_List : Integer_List;
         Result     : Binary_Matrix
           (Y'Range, 1 .. Positive (Self.Classes.Length))
           := (others => (others => 0));
      begin
         for row in Result'Range loop
            Class_List := Classes_Array (row);
            for col in Class_List.First_Index .. Class_List.Last_Index loop
               Result (row, Class_List.Element (col)) := 1;
            end loop;
         end loop;

         return Result;
      end;

   end Transform;

   --  -------------------------------------------------------------------------

   function Transform
     (Self : Label_Binarizer; Y : Array_Of_Integer_Lists)
      return Boolean_Matrix is
      use Integer_Package;
      Routine_Name  : constant String := "Label.Transform Array_Of_Integer_Lists ";
      Y_Row         : Integer_List;
      Classes       : Integer_List;
      Indices       : Integer_List_2D;
      Class_Curs    : Cursor;
      Classes_Array : Array_Of_Integer_Lists (1 .. Y'Length);
   begin
      for row in Y'Range loop
         Y_Row := Y (row);
         Classes.Clear;
         for col in Y_Row.First_Index .. Y_Row.Last_Index loop
            Class_Curs := Self.Classes.Find (Y_Row (col));
            if Class_Curs /= No_Element then
               Classes.Append (Element (Class_Curs));
            else
               Assert (False, Routine_Name & "Class not found");
            end if;
         end loop;

         Indices.Append (Classes);
         Classes_Array (row) := Classes;
      end loop;

      --  Binarize
      declare
         Bool_List : Integer_List;
         Result    : Boolean_Matrix
           (Y'Range, 1 .. Positive (Self.Classes.Length))
           := (others => (others => False));
      begin
         for row in Result'Range loop
            Bool_List := Classes_Array (row);
            for col in Bool_List.First_Index .. Bool_List.Last_Index loop
               Result (row, Bool_List.Element (col)) := True;
            end loop;
         end loop;

         return Result;
      end;

   end Transform;

   --  -------------------------------------------------------------------------
   --  Transform returns labels as normalized encodings
   function Transform (Self : Label_Encoder; Y : Integer_Array)
                       return Natural_Array is
      Labels : Natural_Array (1 .. Y'Length);
   begin
      if Self.Encoder_Kind = Class_Unique then
         Labels := Encode_Utils.Encode (Y, Self.Uniques);
      else
         raise Label_Error with
           "Label.Transform called with invalid encoder type.";
      end if;

      return Labels;

   end Transform;

   --  -------------------------------------------------------------------------
   --  L835
   function Transform (Y : Integer_Array_List) return Binary_Matrix is
      --  Routine_Name  : constant String :=
      --                    "Label.Transform Integer_Array_List ";
      Class_Mapping : Integer_List;
   begin
      return Transform_CM (Y, Class_Mapping);

   end Transform;

   --  -------------------------------------------------------------------------
   --  L835
   function Transform (Y : Integer_Matrix) return Binary_Matrix is
      --        Routine_Name : constant String :=
      --                         "Label.Transform Integer_Matrix ";
      Class_Mapping :  Integer_List;
   begin
      return Transform_CM (Y, Class_Mapping);

   end Transform;

   --  -------------------------------------------------------------------------
   --  L835
   function Transform (Y : Unbounded_String_Array_List) return Binary_Matrix is
      --  Routine_Name  : constant String :=
      --                    "Label.Transform Unbounded_String_Array_List ";
      Class_Mapping : Unbounded_List;
   begin
      return Transform_CM (Y, Class_Mapping);

   end Transform;

   --  -------------------------------------------------------------------------

   function Transform_CM
     (Y             : Integer_Array_List;
      Class_Mapping : in out Integer_List) return Binary_Matrix is
      use Integer_Sorting;
      Routine_Name : constant String :=
                       "Label.Transform_CM Integer_Array_List ";
   begin
      Assert (not Y.Is_Empty, Routine_Name & "Y is empty");
      Class_Mapping.Clear;

      for index in Y.First_Index .. Y.Last_Index loop
         declare
            Values : constant Integer_Array := Y (index);
            Found  : Boolean := False;
         begin
            for col in Values'Range loop
               for key in Class_Mapping.First_Index ..
                 Class_Mapping.Last_Index loop
                  Found := Found or
                    Class_Mapping (key) = Values (col);
               end loop;

               if not Found then
                  Class_Mapping.Append (Values (col));
               end if;
            end loop;
         end;
      end loop;

      Sort (Class_Mapping);
      declare
         Result  : Binary_Matrix
           (1 .. Positive (Y.Length), 1 .. Positive (Class_Mapping.Length))
           := (others => (others => 0));
      begin
         for row in Y.First_Index .. Y.Last_Index loop
            declare
               Values : constant Integer_Array := Y (row);
            begin
               for col in Values'Range loop
                  for key in Class_Mapping.First_Index ..
                    Class_Mapping.Last_Index loop
                     if Values (col) = Class_Mapping.Element (key) then
                        Result (row, key) := 1;
                     end if;
                  end loop;
               end loop;
            end;
         end loop;

         return Result;
      end;

   end Transform_CM;

   --  -------------------------------------------------------------------------

   function Transform_CM (Y             : Integer_Matrix;
                          Class_Mapping : in out Integer_List)
                          return Binary_Matrix is
      use Integer_Sorting;
      --        Routine_Name : constant String :=
      --                         "Label.Transform_CM Integer_Matrix ";
      Found : Boolean := False;
   begin
      Class_Mapping.Clear;

      for row in Y'Range loop
         Found := False;
         for col in Y'Range (2) loop
            for key in Class_Mapping.First_Index ..
              Class_Mapping.Last_Index loop
               Found := Found or
                 Class_Mapping (key) = Y (row, col);
            end loop;

            if not Found then
               Class_Mapping.Append (Y (row, col));
            end if;
         end loop;
      end loop;

      Sort (Class_Mapping);
      declare
         Result  : Binary_Matrix
           (Y'Range, 1 .. Positive (Class_Mapping.Length))
           := (others => (others => 0));
      begin
         for row in Y'Range loop
            for col in Y'Range (2) loop
               for key in Class_Mapping.First_Index ..
                 Class_Mapping.Last_Index loop
                  if Y (row, col) = Class_Mapping.Element (key) then
                     Result (row, key) := 1;
                  end if;
               end loop;
            end loop;
         end loop;

         return Result;
      end;

   end Transform_CM;

   --  -------------------------------------------------------------------------

   function Transform_CM
     (Y             : Unbounded_String_Array_List;
      Class_Mapping : in out Unbounded_List) return Binary_Matrix is
      use Unbounded_Sorting;
      Routine_Name : constant String :=
                       "Label.Transform_CM Unbounded_String_Array_List ";
   begin
      Assert (not Y.Is_Empty, Routine_Name & "Y is empty");
      Class_Mapping.Clear;

      for index in Y.First_Index .. Y.Last_Index loop
         declare
            Values : constant Unbounded_String_Array := Y (index);
            Found  : Boolean := False;
         begin
            for col in Values'Range loop
               for key in Class_Mapping.First_Index ..
                 Class_Mapping.Last_Index loop
                  Found := Found or
                    Class_Mapping (key) = Values (col);
               end loop;

               if not Found then
                  Class_Mapping.Append (Values (col));
               end if;
            end loop;
         end;
      end loop;

      Sort (Class_Mapping);
      declare
         Result  : Binary_Matrix
           (1 .. Positive (Y.Length), 1 .. Positive (Class_Mapping.Length))
           := (others => (others => 0));
      begin
         for row in Y.First_Index .. Y.Last_Index loop
            declare
               Values : constant Unbounded_String_Array := Y (row);
            begin
               for col in Values'Range loop
                  for key in Class_Mapping.First_Index ..
                    Class_Mapping.Last_Index loop
                     if Values (col) = Class_Mapping.Element (key) then
                        Result (row, key) := 1;
                     end if;
                  end loop;
               end loop;
            end;
         end loop;

         return Result;
      end;

   end Transform_CM;

   --  -------------------------------------------------------------------------

   function Transform (Self : UB_Label_Binarizer; Y : Unbounded_String_Array)
                       return Binary_Matrix is
      --        Routine_Name : constant String :=
      --                         "Label.Transform Binarize Unbounded_String_Array Y ";
   begin
      return Label_Binarize (Y, Self.Classes);

   end Transform;

   --  -------------------------------------------------------------------------

   function Transform (Self : UB_Label_Binarizer; Y : Unbounded_String_Matrix)
                       return Binary_Matrix is
      --        Routine_Name : constant String := "Label.Transform Binarize Unbounded_String_Matrix Y ";
   begin
      return Label_Binarize (Y, Self.Classes);

   end Transform;

   --  -------------------------------------------------------------------------

end Label;

--  end Label;
--                    Result (row, col) := 1;
