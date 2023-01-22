
with TensorFlow_C.Binding; use Tensorflow_C.Binding;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Hello_TF is
   Version : String := Value (TF_Version);
begin
   Put_Line ("Hello Tensorflow.");
   Put_Line ("Version: " & Version);
end Hello_TF;
