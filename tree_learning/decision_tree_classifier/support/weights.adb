--  Based on scikit-learn/sklearn/utils/class_weight.py

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;
with Label;
--  with Printing;

package body Weights is

   function Reduce_Weight_Lists (Lists : Weight_Lists_2D)
                                 return Weight_List;

   --  -------------------------------------------------------------------------

   function Compute_Balanced_Class_Weights
     (Classes : ML_Types.Value_Data_List;  Y : ML_Types.Value_Data_List)
      return Weight_List is
      use Label;
      use NL_Types.Natural_Package;
      Weights             : Weight_List;
      LE                  : Label.Label_Encoder (Class_Unique);
--                                                   Integer (Y.Length));
      Y_Index             : NL_Types.Natural_List;
      aWeight             : Float;
      Bins                : NL_Types.Natural_List;
      Recip_Freq          : NL_Types.Float_List;
      Transformed_Classes : NL_Types.Natural_List;
      Scale               : Float;
      Recip_Freq_Index    : Natural;
   begin
      --  Find the weight of each class as present in Y.
      --  Balanced class weights should be given by:
      --  n_samples / (n_classes * Bin_Count (Y)); that is
      --  Y.Length / (Classes.Length * Bin_Count (Y)).
      --  but, this is Recip_Freq
      Y_Index := Label.Fit_Transform (LE, Y);
      Bins := Classifier_Utilities.Bin_Count (Y_Index);
      Scale := Float (Y.Length) / Float (LE.Uniques.Length);
      for index in Bins.First_Index .. Bins.Last_Index loop
         Recip_Freq.Append (Scale / Float (Bins.Element (index)));
      end loop;

      Transformed_Classes := Label.Transform (LE, Classes);
      Weights.Clear;
      for index in Transformed_Classes.First_Index ..
        Transformed_Classes.Last_Index loop
         Recip_Freq_Index := Transformed_Classes.Element (index) + 1;
         aWeight := Float (Recip_Freq.Element (Recip_Freq_Index));
         Weights.Append (aWeight);
      end loop;

      return Weights;

   end Compute_Balanced_Class_Weights;

   --  -------------------------------------------------------------------------

   function Compute_Class_Weights (Weight_Kind   : Weight_Type;
                                   Class_Weights : Weight_List;
                                   Classes       : ML_Types.Value_Data_List;
                                   Y             : ML_Types.Value_Data_List)
                                   return Weight_List is
      Weights : Weight_List;
   begin
      case Weight_Kind is
         when No_Weight =>
            for index in Classes.First_Index .. Classes.Last_Index loop
               Weights.Append (1.0);
            end loop;

         when Balanced_Weight =>
            Weights := Compute_Balanced_Class_Weights (Classes, Y);

         when Weight_Dict => --  user-defined dictionary
            for index in Class_Weights.First_Index .. Class_Weights.Last_Index
            loop
               null;
            end loop;
            raise Weights_Error with
              "Compute_Class_Weights dictionary process not implemented.";

         when Weights_List =>
            Put_Line ("Weights.Compute_Class_Weight.Weights_List");
            --              Weights := Compute_Class_Weights
            --                (Weights_List, Float_Package.Empty_Vector, Classes, Y);
      end case;

      return Weights;

   end Compute_Class_Weights;

   --  -------------------------------------------------------------------------

   function Compute_Balanced_Sample_Weight
     (Y : ML_Types.Value_Data_Lists_2D) return Weight_List is
      use ML_Types;
      Y_1                   : Value_Data_List;
      Y_Full                : Value_Data_List;
      Classes_Full          : Value_Data_List;
      Inverse               : NL_Types.Natural_List :=
                                NL_Types.Natural_Package.Empty_Vector;
      K_Indices             : Classifier_Types.Integer_List;
      Weight_K              : Weight_List;
      aWeight               : Float;
      Weights               : Weight_List;
      Expanded_Class_Weight : Weight_Lists_2D;
   begin
      for index_2 in Y.Element (1).First_Index ..
        Y.Element (1).Last_Index loop
         Y_Full.Clear;
         Weights.Clear;
         for index in Y.First_Index .. Y.Last_Index loop
            Y_1 := Y.Element (index);
            Y_Full.Append (Y_1.Element (index_2));
         end loop;

         Classes_Full := Encode_Utils.Unique (Y_Full, Inverse);
         Weight_K := Compute_Balanced_Class_Weights (Classes_Full, Y_Full);
         --  weight_k = weight_k[np.searchsorted(classes_full, y_full)]
         K_Indices := Classifier_Utilities.Search_Sorted_Value_List
           (Classes_Full, Y_Full);
         for y_index in Y_Full.First_Index .. Y_Full.Last_Index loop
            aWeight := Weight_K.Element (K_Indices.Element (y_index));
            Weights.Append (aWeight);
         end loop;
         Expanded_Class_Weight.Append (Weights);
      end loop;

      return Reduce_Weight_Lists (Expanded_Class_Weight);

   end Compute_Balanced_Sample_Weight;

   --  -------------------------------------------------------------------------
   --  Compute_Sample_Weight estimates sample weights by class for
   --  unbalanced datasets.
   --  y : array-like of shape (n_samples,) or (n_samples, n_outputs)
   --   Array of original class labels per sample.
   --  Class_Weight : dict, list of dicts, "balanced", or None, optional
   --     Weights associated with classes
   --  Indices : list of indices to be used in a subsample
   function Compute_Sample_Weight (Weight_Kind    : Weight_Type;
                                   Y              : ML_Types.Value_Data_Lists_2D;
                                   Class_Weights  : Weight_List :=
                                     NL_Types.Float_Package.Empty_Vector;
                                   Indices        : ML_Types.Integer_List :=
                                     ML_Types.Integer_Package.Empty_Vector)
                                   return Weight_List is
      use ML_Types;
      use Value_Data_Package;
      Num_Outputs           : constant Integer := Integer (Y.Length);
      Y_Full                : Value_Data_List;
      Classes               : Value_Data_List;
      Classes_Full          : Value_Data_List;
      aClass                : Value_Record;
      Classes_Missing       : Value_Data_List;
      Class_Weight_K        : Float;
      Class_Weight_K_List   : Weight_List;
      Y_Subsample           : Value_Data_List;
      Classes_Subsample     : Value_Data_List;
      Weight_K              : Weight_List;
      aWeight               : Float;
      K_Indices             : Classifier_Types.Integer_List;
      Inverse               : NL_Types.Natural_List;
      Class_K_Weights       : Weight_List;
      Expanded_Class_Weight : Weight_Lists_2D;
      Result                : Weight_List;
   begin
      case Weight_Kind is
         when Balanced_Weight =>
            --                  Put_Line ("Compute_Sample_Weight Balanced_Weight");
            Result := Compute_Balanced_Sample_Weight (Y);

         when No_Weight =>
            Put_Line ("Compute_Sample_Weight No_Weight");
            for index_k in 1 .. Num_Outputs loop
               for index in 1 .. Y.Element (1).Length loop
                  Result.Append (1.0);
               end loop;
            end loop;

         when Weight_Dict =>
            raise Weights_Error with
              "Weights.Compute_Sample_Weight Dictionary is not implemented";

         when Weights_List =>
            Put_Line ("Compute_Sample_Weight Weights_List");
            Inverse.Clear;
            if Num_Outputs > 1 and then
              Integer (Class_Weights.Length) /= Num_Outputs
            then
               raise Weights_Error with
                 "Weights.Compute_Sample_Weight; For multi-output, number of " &
                 "elements in class_weight should match number of outputs.";
            end if;

            for index_k in 1 .. Num_Outputs loop
               --  y_full = y[:, k]
               Put_Line ("Compute_Sample_Weight index_k: " &
                           Integer'Image (index_k));
               Y_Full := Y.Element (index_k);
               Classes_Full := Encode_Utils.Unique (Y_Full, Inverse);
               Classes_Missing.Clear;
--                 Printing.Print_Value_Data_List
--                   ("Compute_Sample_Weight Y_Full", Y_Full);
--                 Printing.Print_Value_Data_List
--                   ("Compute_Sample_Weight Classes_Full", Classes_Full);

               if Weight_Kind = Balanced_Weight or Num_Outputs = 1 then
                  --  Should be only one Class_Weight
                  Class_Weight_K := Class_Weights.Element (1);
               else
                  Class_Weight_K := Class_Weights.Element (index_k);
               end if;

               --  Compute_Class_Weights input parameter is a Weight_List
               Class_Weight_K_List.Clear;
               Class_Weight_K_List.Append (Class_Weight_K);

               if Indices.Is_Empty then
                  Weight_K := Compute_Class_Weights
                    (Weight_Kind, Class_Weight_K_List, Classes_Full, Y_Full);
               else
                  --  Get class weights for the subsample covering all classes in
                  --  case some labels present in the original data are missing
                  --  from the sample.

                  Put_Line ("Compute_Sample_Weight Indices not Empty.");
                  for index in Indices.First_Index .. Indices.Last_Index loop
                     Y_Subsample.Append (Y.Element (Indices.Element (index)));
                  end loop;

                  Classes_Subsample := Encode_Utils.Unique (Y_Subsample, Inverse);
                  --  weight_k = np.take
                  --    (compute_class_weight
                  --       (class_weight_k, classes=classes_subsample, y=y_subsample),
                  --     np.searchsorted(classes_subsample, classes_full),
                  --     mode='clip')
                  Class_K_Weights := Compute_Class_Weights
                    (Weight_Kind, Class_Weight_K_List, Y_Subsample, Classes_Subsample);
                  --              K_Indices := Classifier_Utilities.Search_Sorted_Value_List
                  --                (Classes, Y_Full);
--                    Printing.Print_Weights
--                      ("Compute_Sample_Weight Indices Class_K_Weights", Class_K_Weights);
                  K_Indices := Classifier_Utilities.Search_Sorted_Value_List
                    (Classes, Y_Full);
--                    Printing.Print_Integer_List
--                      ("Compute_Sample_Weight Indices K_Indices", K_Indices);

                  Weight_K.Clear;
                  for index in Class_K_Weights.First_Index ..
                    Class_K_Weights.Last_Index loop
                     Weight_K.Append (Class_K_Weights (K_Indices (index)));
                  end loop;

                  for index in Classes.First_Index .. Classes.Last_Index loop
                     aClass := Classes (index);
                     if Find (Classes_Subsample, aClass) =
                       Value_Data_Package.No_Element
                     then
                        Classes_Missing.Append (aClass);
                     end if;
                  end loop;
               end if;

--                 Printing.Print_Weights
--                   ("Compute_Sample_Weight Indices Weight_K", Weight_K);
               --  weight_k = weight_k[np.searchsorted(classes_full, y_full)]

               K_Indices := Classifier_Utilities.Search_Sorted_Value_List
                 (Classes_Full, Y_Full);
--                 Printing.Print_Integer_List
--                   ("Compute_Sample_Weight K_Indices", K_Indices);
               Class_K_Weights.Clear;
               for index in K_Indices.First_Index .. K_Indices.Last_Index loop
                  if index <= Weight_K.Last_Index then
                     aWeight := Weight_K.Element (index);
                     Put_Line ("Compute_Sample_Weight aWeight" & Float'Image (aWeight));
                     if not Class_K_Weights.Contains (aWeight) then
                        Put_Line ("Compute_Sample_Weight add aWeight to Class_K_Weights");
                        Class_K_Weights.Append (aWeight);
                     end if;
                  end if;
               end loop;
--                 Printing.Print_Weights
--                   ("Compute_Sample_Weight Indices Class_K_Weights", Class_K_Weights);

               if not Classes_Missing.Is_Empty then
                  --  Make missing classes weights zero
                  for index in Y_Full.First_Index .. Y_Full.Last_Index loop
                     aClass := Y_Full (index);
                     if not (Find (Classes_Missing, aClass) =
                               Value_Data_Package.No_Element)
                     then
                        aWeight := 0.0;
                        Weight_K.Replace_Element (index, aWeight);
                     end if;
                  end loop;
               end if;

               Expanded_Class_Weight.Append (Weight_K);
            end loop;
            Result := Reduce_Weight_Lists (Expanded_Class_Weight);

      end case;

      return Result;

   end Compute_Sample_Weight;

   --  -------------------------------------------------------------------------

   function Get_Column (Weights  : Weight_Lists_2D; Data_Index : Positive)
                        return Weight_List is
      aList  : Weight_List;
      Column : Weight_List;
      Data   : Float;
   begin
      for index in 1 .. integer (Weights.Length) loop
         aList := Weights.Element (index);
         Data := aList.Element (Data_Index);
         Column (index) := Data;
      end loop;

      return Column;

   end Get_Column;

   --  ------------------------------------------------------------------------

   function Get_Column (Weights  : Weight_Lists_3D; Data_Index : Positive)
                        return Weight_Lists_2D is
      aList_2D  : Weight_Lists_2D;
      Column_2D : Weight_Lists_2D;
      Data_2D   : Weight_List;
   begin
      for index in 1 .. Integer (Weights.Length) loop
         aList_2D := Weights.Element (index);
         Data_2D := aList_2D.Element (Data_Index);
         Column_2D (index) := Data_2D;
      end loop;

      return Column_2D;

   end Get_Column;

   --  -------------------------------------------------------------------------
   --  Max returns the indices of the max element of Weights in axis 1
   --      function Max (Weights : Weight_Lists_2D; Data_Index : Index_Range_2D)
   --                    return Natural_List is
   --          Data        : Weight_Lists_2D;
   --          Row         : Weight_List;
   --          Value       : Float;
   --          Max_Value   : Float;
   --          Max_Index   : Positive;
   --          Max_Indices : Natural_List;
   --      begin
   --          if Data_Index = 1 then
   --              Data := Transpose (Weights);
   --          else
   --              Data := Weights;
   --          end if;
   --
   --          for index_1 in Data.First_Index .. Data.Last_Index loop
   --              Row := Data.Element (index_1);
   --              Max_Value := -Float'Last;
   --              Max_Index := 1;
   --              for index_2 in Row.First_Index .. Row.Last_Index loop
   --                  Value := Row.Element (index_2);
   --                  if Value > Max_Value then
   --                      Max_Value := Value;
   --                      Max_Index := index_2;
   --                  end if;
   --              end loop;
   --              Max_Indices.Append (Max_Index);
   --          end loop;
   --
   --          return Max_Indices;
   --
   --      end Max;

   --  -------------------------------------------------------------------------

   function Max (Weights : Weight_List) return Positive is
      Value     : Float;
      Max_Value : Float := -Float'Last;
      Max_Index : Positive := 1;
   begin
      for index in Weights.First_Index .. Weights.Last_Index loop
         Value := Weights.Element (index);
         if Value > Max_Value then
            Max_Value := Value;
            Max_Index := index;
         end if;
      end loop;

      return Max_Index;

   end Max;

   --  -------------------------------------------------------------------------

   function Max (Weights : Weight_List) return Float is
      Value     : Float;
      Max_Value : Float := -Float'Last;
   begin
      for index in Weights.First_Index .. Weights.Last_Index loop
         Value := Weights.Element (index);
         if Value > Max_Value then
            Max_Value := Value;
         end if;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

   function Max (Weights_3D : Weight_Lists_3D) return Float is
      List_2D   : Weight_Lists_2D;
      Weights   : Weight_List;
      Value     : Float;
      Max_Value : Float := -Float'Last;
   begin
      for index_1 in Weights_3D.First_Index .. Weights_3D.Last_Index loop
         List_2D := Weights_3D.Element (index_1);
         for index_2 in List_2D.First_Index .. List_2D.Last_Index loop
            Weights := List_2D.Element (index_2);
            for index_3 in Weights.First_Index .. Weights.Last_Index loop
               Value := Weights.Element (index_3);
               if Value > Max_Value then
                  Max_Value := Value;
               end if;
            end loop;
         end loop;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

   function Min (Weights : Weight_List) return Positive is
      Value     : Float;
      Min_Value : Float := Float'Last;
      Min_Index : Positive := 1;
   begin
      for index in Weights.First_Index .. Weights.Last_Index loop
         Value := Weights.Element (index);
         if Value < Min_Value then
            Min_Value := Value;
            Min_Index := index;
         end if;
      end loop;

      return Min_Index;

   end Min;

   --  -------------------------------------------------------------------------

   function Min (Weights : Weight_List) return Float is
      Value     : Float;
      Min_Value : Float := Float'Last;
   begin
      for index in Weights.First_Index .. Weights.Last_Index loop
         Value := Weights.Element (index);
         if Value < Min_Value then
            Min_Value := Value;
         end if;
      end loop;

      return Min_Value;

   end Min;

   --  -------------------------------------------------------------------------

   function Min (Weights_3D : Weight_Lists_3D) return Float is
      List_2D   : Weight_Lists_2D;
      Weights   : Weight_List;
      Value     : Float;
      Min_Value : Float := Float'Last;
   begin
      for index_1 in Weights_3D.First_Index .. Weights_3D.Last_Index loop
         List_2D := Weights_3D.Element (index_1);
         for index_2 in List_2D.First_Index .. List_2D.Last_Index loop
            Weights := List_2D.Element (index_2);
            for index_3 in Weights.First_Index .. Weights.Last_Index loop
               Value := Weights.Element (index_3);
               if Value < Min_Value then
                  Min_Value := Value;
               end if;
            end loop;
         end loop;
      end loop;

      return Min_Value;

   end Min;

   --  -------------------------------------------------------------------------

   function Reduce_Weight_Lists (Lists : Weight_Lists_2D)
                                 return Weight_List is
      use Ada.Containers;
      List_Length  : constant Count_Type := Lists.Element (1).Length;
      aList        : Weight_List;
      Value        : Float;
      Reduced_List : Weight_List;
   begin
      for index in 1 .. List_Length loop
         Reduced_List.Append (1.0) ;
      end loop;

      for index in Lists.First_Index .. Lists.Last_Index loop
         aList := Lists.Element (index);
         for index_2 in 1 .. Integer (List_Length) loop
            Value := Reduced_List.Element (index_2) *
              aList.Element (index_2);
            Reduced_List.Replace_Element (index_2, Value);
         end loop;
      end loop;

      return Reduced_List;

   end Reduce_Weight_Lists;

   --  -------------------------------------------------------------------------

   function Transpose (Weights : Weight_Lists_2D) return Weight_Lists_2D is
      New_Row    : Weight_List;
      Result     : Weight_Lists_2D;
   begin
      for col in 1 .. Positive (Weights.Element (1).Length) loop
         New_Row := Get_Column (Weights, col);
         Result.Append (New_Row);
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

end Weights;
