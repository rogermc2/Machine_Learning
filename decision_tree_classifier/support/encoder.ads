
with Classifier_Types; use Classifier_Types;
with ML_Types;

package Encoder is

   type Label_Encoder is record
      --  Encode target labels with value between 0 and n_classes-1.
      --  This transformer should be used to encode target values, *i.e.* `y`, and
      --  not the input `X`.
      Classes : Integer_List;
   end record;

   function Fit (Self : Label_Encoder; Y : ML_Types.Value_Data_List)
                 return Label_Encoder;
   function Fit_Transform (Self : in out Label_Encoder;
                           X    : Sample_Matrix;
                           Y    : ML_Types.Value_Data_List :=
                             ML_Types.Value_Data_Package.Empty_Vector)
                           return Sample_Matrix;

end Encoder;
