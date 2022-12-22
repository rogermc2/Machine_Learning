
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ML_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Basic_Printing is

   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix;
                                  Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Byte_Array (Name  : String; anArray : Byte_Array;
                               Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Matrix (Name  : String; aMatrix : Real_Float_Matrix;
                                 Start : Positive := 1; Finish : Natural := 0;
                                 Col_Start  : Positive := 1;
                                 Col_Finish : Natural := 0);
   procedure Print_Float_Matrix_List
      (Name  : String; aList : Real_Matrix_List;
       Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Vector (Name : String; Vec : Real_Float_Vector;
                                 Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Vector_As_Line
     (Name : String; Vec : Real_Float_Vector;  Start : Positive := 1;
      Finish : Natural := 0);
   procedure Print_Integer_Array (Name  : String; anArray : Integer_Array;
                                  Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Integer_List (Name  : String;
                                 aList : ML_Types.Integer_List);
   procedure Print_Integer_Map (Name : String;
                                aMap : ML_Types.Integer_Label_Map);
   procedure Print_Integer_Matrix (Name : String; aMatrix : Integer_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Binary_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Unsigned_8_Array_3D);
   procedure Print_Unbound_Array (Name : String;
                                   UB   : Unbounded_String_Array);
   procedure Print_Unbound_List (Name : String; UB : ML_Types.Unbounded_List);
   procedure Print_Unbound_Matrix (Name : String;
                                   UB   : Unbounded_String_Matrix);

end Basic_Printing;
