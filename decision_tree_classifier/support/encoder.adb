
package body Encoder is

   --  -------------------------------------------------------------------------

   function Encode (Values : ML_Types.Value_Data_List) return Integer_List is
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

   function Fit (Self : Label_Encoder; Y : ML_Types.Value_Data_List)
                 return Label_Encoder is
      theEncoder : Label_Encoder;
   begin
      theEncoder.Classes := Encode (Y);
      return theEncoder;
   end Fit;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   --  Y ignored
   function Fit_Transform (Self : in out Label_Encoder;
                           X    : Sample_Matrix;
                           Y    : ML_Types.Value_Data_List :=
                             ML_Types.Value_Data_Package.Empty_Vector)
                           return Sample_Matrix is
      X_Out : Sample_Matrix := X;
   begin
      --        Self.Classes := Y_New;
      return X_Out;
   end Fit_Transform;

   --  -------------------------------------------------------------------------

end Encoder;
