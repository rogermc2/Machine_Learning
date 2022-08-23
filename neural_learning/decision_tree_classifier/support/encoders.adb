--  Based on scikit-learn/sklearn/preprocessing/_encoders.py

package body Encoders is

   --  -------------------------------------------------------------------------

   function Encode (Values : Value_Data_List) return Integer_List is
      Codes : Integer_List;
      Code  : Natural:= 0;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Code := Code + 1;
         Codes.Append (Code);
      end loop;
      return Codes;
   end Encode;

   --  -------------------------------------------------------------------------

   function Fit (Self : One_Hot_Encoder; Y : Value_Data_List)
                 return One_Hot_Encoder is
      pragma Unreferenced (Self);
      theEncoder : One_Hot_Encoder;
   begin
      theEncoder.Attributes.Num_Values := Encode (Y);
      return theEncoder;
   end Fit;

   --  -------------------------------------------------------------------------

--     function Fit (Self : One_Hot_Encoder; X : Sample_Matrix)
--                   return One_Hot_Encoder is
--        pragma Unreferenced (Self);
--        theEncoder : One_Hot_Encoder;
--     begin
--        theEncoder.Attributes.Num_Values := Encode (Y);
--        return theEncoder;
--     end Fit;

   --  -------------------------------------------------------------------------

   --  Fit_Transform fits label encoder and returns encoded labels
   --  Y ignored
--     function Fit_Transform (Self : in out One_Hot_Encoder;
--                             X    : Sample_Matrix;  --   [n_samples, n_features]
--                             Y    : Value_Data_List :=
--                               Value_Data_Package.Empty_Vector)
--                             return Sample_Matrix is
--        X_Out : Sample_Matrix;
--     begin
--        return X_Out;
--     end Fit_Transform;

   --  -------------------------------------------------------------------------

   procedure Init (Self                 : in out One_Hot_Encoder;
                   Categories           : Value_Data_List;
                   Drop                 : Value_Data_List;
                   Data_Kind            : Data_Type;
                   Num_Values           : Integer_List;
                   Categorical_Features : Value_Data_List;
                   Auto_Category        : Boolean := False;
                   Drop_First           : Boolean := False;
                   Num_Values_Auto      : Boolean := True;
                   All_Cat_Features     : Boolean := True) is
   begin
      Self.Parameters.Categories := Categories;
      Self.Parameters.Drop := Drop;
      Self.Parameters.Data_Kind := Data_Kind;
      Self.Parameters.Num_Values := Num_Values;
      Self.Parameters.Categorical_Features := Categorical_Features;
      Self.Parameters.Auto_Category := Auto_Category;
      Self.Parameters.Drop_First := Drop_First;
      Self.Parameters.Num_Values_Auto := Num_Values_Auto;
      Self.Parameters.All_Cat_Features := All_Cat_Features;
   end Init;

   --  -------------------------------------------------------------------------

end Encoders;
