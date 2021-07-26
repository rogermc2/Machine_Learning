
package body Encoder is

   --  -------------------------------------------------------------------------

   function Encode (Values : Integer_List) return Integer_List is
   begin
      return Unique (Values);
   end Encode;

   --  -------------------------------------------------------------------------

   function Fit (Self : Label_Encoder; Y : Integer_List) return Label_Encoder is
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
                           Y    : Integer_List := Integer_Package.Empty_Vector)
                           return Sample_Matrix is
      X_Out : Sample_Matrix := X;
   begin
--        Self.Classes := Y_New;
      return X_Out;
   end Fit_Transform;

   --  -------------------------------------------------------------------------

end Encoder;
