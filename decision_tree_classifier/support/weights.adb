
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Label;
--  with Utilities;

package body Weights is

   function Get_Column (Weights  : Weight_Lists_List; Data_Index : Positive;
                        Data     : out Float) return  Float_Array;
   function Reduce_Weight_Lists (Lists : Weight_Lists_List)
                                 return Weight_List;

   --  -------------------------------------------------------------------------
   --  Compute_Class_Weight estimates class weights for unbalanced datasets.
   function Compute_Class_Weights (Weight_Kind   : Weight_Type;
                                   Class_Weights : Weight_List;
                                   Classes       : ML_Types.Value_Data_List;
                                   Y             : ML_Types.Value_Data_List)
                                   return Weight_List is
      use ML_Types;
      Weights          : Weight_List;
      LE               : Label.Label_Encoder;
      Y_Ind            : Natural_List;
      aWeight          : Float;
      aClass           : Value_Record;
      Recip_Freq       : Natural_List;
      Recip_Freq2      : Natural_List;
      Recip_Freq_Index : Positive;
      Recip            : Natural;
      Scale            : Natural;
   begin
      if Weight_Kind = No_Weight then
         for index in Classes.First_Index .. Classes.Last_Index loop
            Weights.Append (1.0);
         end loop;

      elsif Weight_Kind = Balanced_Weight then
         --  Find the weight of each class  present in Y.
         Y_Ind := Label.Fit_Transform (LE, Y);
         Scale := Natural (Float (Y.Length) / Float (LE.Classes.Length));
         Recip_Freq := Classifier_Utilities.Bin_Count (Y_Ind);
         for index in Recip_Freq.First_Index .. Recip_Freq.Last_Index loop
            Recip := Scale * Recip_Freq.Element (index);
            Recip_Freq.Replace_Element (index, Recip);
         end loop;

         Recip_Freq2 := Label.Transform (LE, Classes);
         for index in Recip_Freq2.First_Index .. Recip_Freq2.Last_Index loop
            Recip_Freq_Index := Recip_Freq2.Element (index);
            aClass := Classes.Element (Recip_Freq_Index);
            case aClass.Value_Kind is
               when Boolean_Type | UB_String_Type =>
                  raise Weights_Error with
                    "Weights.Compute_Class_Weights invalid class type :" &
                    Data_Type'Image (aClass.Value_Kind);
               when Float_Type =>
                  aWeight := aClass.Float_Value;
               when Integer_Type =>
                  aWeight := Float (aClass.Integer_Value);
            end case;
            Weights.Append (aWeight);
         end loop;

      else  --  user-defined dictionary
         for index in Class_Weights.First_Index .. Class_Weights.Last_Index
         loop
            null;
         end loop;
         raise Weights_Error with
           "Weights.Compute_Class_Weights dictionary process not implemented.";
      end if;

      return Weights;

   end Compute_Class_Weights;

   --  -------------------------------------------------------------------------
   --  Compute_Sample_Weight estimates sample weights by class for
   --  unbalanced datasets.
   --  y : array-like of shape (n_samples,) or (n_samples, n_outputs)
   --   Array of original class labels per sample.
   --  Class_Weight : dict, list of dicts, "balanced", or None, optional
   --     Weights associated with classes
   --  Indices : list of indices to be used in a subsample
   function Compute_Sample_Weight (Weight_Kind    : Weight_Type;
                                   Y              : ML_Types.List_Of_Value_Data_Lists;
                                   Class_Weights  : Weight_List :=
                                     Float_Package.Empty_Vector;
                                   Indices        : Integer_List :=
                                     Integer_Package.Empty_Vector)
                                   return Weight_List is
      use ML_Types;
      use Value_Data_Package;
      Num_Outputs           : constant Integer := Integer (Y.Length);
      Y_Full                : Value_Data_List;
      Classes_Full          : Value_Data_List;
      aClass                : Value_Record;
      Classes_Missing       : Value_Data_List;
      Class_Weight_K        : Weight_List;
      Y_Subsample           : Value_Data_List;
      Classes_Subsample     : Value_Data_List;
      Weight_K              : Weight_List;
      aWeight               : Float;
      K_Indices             : Integer_List;
      Class_K_Weights       : Weight_List;
      Expanded_Class_Weight : Weight_Lists_List;
   begin
      if Weight_Kind /= Balanced_Weight then
         raise Weights_Error with
           "Compute_Sample_Weight; Weight does not contain the only" &
           " valid preset for class_weight which is balanced.";
      end if;

      if Num_Outputs > 1 and then
        Integer (Class_Weights.Length) /= Num_Outputs then
         raise Weights_Error with
           "Weights.Compute_Sample_Weight; For multi-output, number of " &
           "elements in class_weight should match number of outputs.";
      end if;

      for index_k in 1 .. Num_Outputs loop
         --  y_full = y[:, k]
         Y_Full := Classifier_Utilities.Get_Column (Y, index_k);
         Classes_Full := Classifier_Utilities.Unique_Values (Y_Full);
         Classes_Missing.Clear;

         if Weight_Kind = Balanced_Weight or Num_Outputs = 1 then
            Class_Weight_K := Class_Weights;
         else
            Class_Weight_K.Clear;
            Class_Weight_K.Append (Class_Weights.Element (index_k));
         end if;

         if Indices.Is_Empty then
            Weight_K := Compute_Class_Weights
              (Weight_Kind, Class_Weight_K, Y_Subsample, Classes_Subsample);
         else
            for index in Indices.First_Index .. Indices.Last_Index loop
               Y_Subsample.Append (Y.Element (Indices.Element (index)));
            end loop;

            Classes_Subsample := Classifier_Utilities.Unique_Values (Y_Subsample);
            --  Get class weights for the subsample covering all classes in
            --  case some labels present in the original data are missing
            --  from the sample.

            --  weight_k = np.take
            --    (compute_class_weight
            --       (class_weight_k, classes=classes_subsample, y=y_subsample),
            --     np.searchsorted(classes_subsample, classes_full),
            --     mode='clip')
            Class_K_Weights := Compute_Class_Weights
              (Weight_Kind, Class_Weight_K, Y_Subsample, Classes_Subsample);
            K_Indices := Classifier_Utilities.Search_Sorted_Value_List
              (Classes_Full, Y_Full);
            Weight_K.Clear;
            for index in Class_K_Weights.First_Index ..
              Class_K_Weights.Last_Index loop
               Weight_K.Append (Class_K_Weights (K_Indices (index)));
            end loop;

            for index in Classes_Full.First_Index ..
              Classes_Full.Last_Index loop
               aClass := Classes_Full (index);
               if Find (Classes_Subsample, aClass) =
                 Value_Data_Package.No_Element then
                  Classes_Missing.Append (aClass);
               end if;
            end loop;
         end if;

         --  weight_k = weight_k[np.searchsorted(classes_full, y_full)]
         Class_K_Weights := Weight_K;
         for index in Weight_K.First_Index .. Weight_K.Last_Index
         loop
            Weight_K (index) := Class_K_Weights (K_Indices (index));
         end loop;

         if not Classes_Missing.Is_Empty then
            --  Make missing classes weights zero
            for index in Y_Full.First_Index .. Y_Full.Last_Index loop
               aClass := Y_Full (index);
               if not (Find (Classes_Missing, aClass) =
                         Value_Data_Package.No_Element) then
                  --                    aWeight := Weight_K (index);
                  aWeight := 0.0;
                  Weight_K.Replace_Element (index, aWeight);
               end if;
            end loop;
         end if;

         Expanded_Class_Weight.Append (Weight_K);
      end loop;

      return Reduce_Weight_Lists (Expanded_Class_Weight);

   end Compute_Sample_Weight;

   --  -------------------------------------------------------------------------

   function Get_Column (Weights  : Weight_Lists_List; Data_Index : Positive;
                        Data     : out Float)
     --                          Data     : out Weight_Data)
                        return  Float_Array is
      aList  : Weight_List;
      Column : Float_Array (1 .. integer (Weights.Length));
   begin
      for index in 1 .. integer (Weights.Length) loop
         aList := Weights.Element (index);
         Data := aList.Element (Data_Index);
         Column (index) := Data;
      end loop;
      return Column;
   end Get_Column;

   --  -------------------------------------------------------------------------

   function Reduce_Weight_Lists (Lists : Weight_Lists_List)
                                 return Weight_List is
      Col     : Float_Array (Lists.First_Index .. Lists.Last_Index);
      Product : Float;
      theList : Weight_List;
      Data    : Float;
   begin
      for index in Lists.First_Index .. Lists.Last_Index loop
         Col := Get_Column (Lists, index, Data);
         Product := 1.0;
         for col_index in Col'Range loop
            Product := Product * Col (col_index);
         end loop ;
         Data := Product;
         theList.Append (Data);
      end loop;
      return theList;
   end Reduce_Weight_Lists;

   --  -------------------------------------------------------------------------

end Weights;
