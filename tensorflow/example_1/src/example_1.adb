--  Reference: https://www.tensorflow.org/api_docs/python/tf/Tensor

with System;

with Interfaces; use Interfaces;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

with Swig.Pointers; use Swig.Pointers;
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
   Dimensions   : aliased Integer_64 := 1;
   Dims_Ptr     : int64_t_Pointer := Dimensions'Unchecked_Access;
   Num_Dims     : int := 1;
   T_Length     : size_t := 1;
--     Data         : Swig.void_ptr;
--     Dealloc      : TensorFlow_C.Deallocator_T;
--     Dealloc_arg  : swig.void_ptr;
   --  A Tensor represents a multidimensional array of elements
   L            : TF_Tensor_Pointer :=
                    TF_AllocateTensor (TF_INT32, Dims_Ptr, Num_Dims, T_Length);
--     M            : TF_Tensor_Pointer :=
--                      TF_CreateNewTensor (TF_INT32, Dims_Ptr, Num_Dims, Data,
--                                          T_Length, Dealloc, Dealloc_arg);
begin

   Put_Line (Program_Name);
   Put_Line ("Version: " & Version);

end Example_1;
