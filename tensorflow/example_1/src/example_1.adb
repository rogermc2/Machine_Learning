
with Interfaces; use Interfaces;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with TensorFlow_C; use Tensorflow_C;
with TensorFlow_C.Binding; use Tensorflow_C.Binding;
with TensorFlow_C.Pointers; use TensorFlow_C.Pointers;

procedure Example_1 is
   Program_Name : constant String := "Example_1 ";
   Version      : constant String := Value (TF_Version);
   Graph        : TF_Graph_Pointer := TF_NewGraph;
   Status       : TF_Status_Pointer := TF_NewStatus;
   Session_Opts : TF_SessionOptions_Pointer := TF_NewSessionOptions;
   Run_Opts     : TF_Buffer_Pointer := null;
begin
   Put_Line (Program_Name);
   Put_Line ("Version: " & Version);
end Example_1;
