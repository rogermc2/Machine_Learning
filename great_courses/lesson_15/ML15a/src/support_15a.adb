
with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with To_BMP;

package body Support_15A is

   function Concatenate (X1, X2 : Integer_Array) return Integer_Array;
   function Concatenate (X1, X2 : Image_Vector) return Image_Vector;
   procedure Read_Cats (Cats_Dir        : String_9_Array;
                        Label           : Natural;
                        Num_Samples     : Positive;
                        Train_X, Test_X : out Image_Vector;
                        Train_Y, Test_Y : out Integer_Array);
   procedure Train_Test_Split
     (X                     : Image_Vector; Y : Integer_Array;
      Train_X               : out Image_Vector; Train_Y : out Integer_Array;
      Test_X                : out Image_Vector; Test_Y : out Integer_Array);

   --  -------------------------------------------------------------------------

   procedure Build_Data (Num_Samples, Train_Size, Test_Size : Positive;
                         Train_X, Test_X                    : out Image_Vector;
                         Train_Y, Test_Y                    : out Integer_Array) is

      Cats_1         : constant String_9_array (1 .. 36) :=
                         ("n01443537", "n01629819", "n01641577", "n01644900", "n01698640", "n01742172",
                          "n01855672", "n01882714", "n02002724", "n02056570", "n02058221", "n02074367",
                          "n02085620", "n02094433", "n02099601", "n02099712", "n02106662", "n02113799",
                          "n02123045", "n02123394", "n02124075", "n02125311", "n02129165", "n02132136",
                          "n02364673", "n02395406", "n02403003", "n02410509", "n02415577", "n02423022",
                          "n02437312", "n02480495", "n02481823", "n02486410", "n02504458", "n02509815");
      Cats_2         : constant String_9_array (1 .. 14) :=
                         ("n01770393", "n01774384", "n01774750", "n01784675", "n02165456", "n02190166",
                          "n02206856", "n02226429", "n02231487", "n02233338", "n02236044", "n02268443",
                          "n02279972", "n02281406");
      Train_X1       :  Image_Vector (1 .. Train_Size);
      Train_Y1       :  Integer_Array (1 .. Train_Size);
      Test_X1        :  Image_Vector (1 .. Test_Size);
      Test_Y1        :  Integer_Array (1 .. Test_Size);
      Train_X2       :  Image_Vector (1 .. Train_Size);
      Train_Y2       :  Integer_Array (1 .. Train_Size);
      Test_X2        :  Image_Vector (1 .. Test_Size);
      Test_Y2        :  Integer_Array (1 .. Test_Size);
   begin
      Read_Cats (Cats_1, 0, Num_Samples, Train_X1, Test_X1, Train_Y1, Test_Y1);
      Read_Cats (Cats_2, 1, Num_Samples, Train_X2, Test_X2, Train_Y2, Test_Y2);

      Train_X := Concatenate (Train_X1, Train_X2);
      Test_X := Concatenate (Test_X1, Test_X2);
      Train_Y := Concatenate (Train_Y1, Train_Y2);
      Test_Y := Concatenate (Test_Y1, Test_Y2);

   end Build_Data;

   --  -------------------------------------------------------------------------

   function Concatenate (X1, X2 : Integer_Array) return Integer_Array is

      X : Integer_Array (1 .. X1'Length + X2'Length);
   begin
      for index in X'Range loop
         if index <= X1'Length then
            X (index) := X1  (index);
         else
            X (index) := X2 (index - X1'Length);
         end if;
      end loop;

      return X;

   end Concatenate;

   --  -------------------------------------------------------------------------

   function Concatenate (X1, X2 : Image_Vector) return Image_Vector is

      X : Image_Vector (1 .. X1'Length + X2'Length);
   begin
      for index in X'Range loop
         if index <= X1'Length then
            X (index) := X1  (index);
         else
            X (index) := X2 (index - X1'Length);
         end if;
      end loop;

      return X;

   end Concatenate;

   --  -------------------------------------------------------------------------

   function Get_Picture (File_Name : String; Show_Name : Boolean := True)
                         return ML_U8_Types.Unsigned_8_Array_3D is
      use Ada.Characters.Handling;
      use ML_U8_Types;
      Routine_Name    : constant String := "Support_5A.Get_Picture ";
      File_Name_Upper : constant String := To_Upper (File_Name);
      File_Kind       : constant String :=
                          File_Name_Upper
                            (File_Name_Upper'Last - 4 .. File_Name_Upper'Last);
   begin
      if File_Kind = ".PNG" then
         declare
            Initial : constant Unsigned_8_Array_3D :=
                        Unsigned_8_Array_3D (To_BMP.Process (File_Name));
            Clipped : Unsigned_8_Array_3D
              (1 .. Initial'Length - 15, 1 .. Initial'Length (2) - 1,
               Initial'Range (3));
         begin
            for row in Clipped'Range loop
               for col in Clipped'Range (2) loop
                  for pix in Clipped'Range (3) loop
                     Clipped (row, col, pix) := Initial (row, col, pix);
                  end loop;
               end loop;
            end loop;

            return Clipped;
         end;

      elsif File_Kind = ".JPG" then
         return Unsigned_8_Array_3D (To_BMP.Process (File_Name, Show_Name));

      elsif File_Kind = ".JPEG" then
         return Unsigned_8_Array_3D (To_BMP.Process (File_Name, Show_Name));

      else
         Put_Line (Routine_Name & "unsupported image format " & File_Kind);
         declare
            Dummy : constant Unsigned_8_Array_3D (1 .. 1, 1 .. 1, 1 .. 1) :=
                      (1 => (1 => (1 => 0)));
         begin
            return Dummy;
         end;
      end if;

   end Get_Picture;

   --  -------------------------------------------------------------------------

   function Max (Values : Real_Float_Vector) return Float is
      Max_Value : Float := Values (Values'First);
   begin
      for row in Values'Range loop
         if Values (row) > Max_Value then
            Max_Value := Values (row);
         end if;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

   procedure Read_Cats (Cats_Dir            : String_9_Array;
                        Label               : Natural;
                        Num_Samples         : Positive;
                        Train_X, Test_X     : out Image_Vector;
                        Train_Y, Test_Y     : out Integer_Array) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use ML_U8_Types;
      Routine_Name    : constant String := "Support_15A.Read_Cats ";
      Train_Directory : constant String :=
                          "../../../../imgs/tiny-imagenet-200/train/";
      Images          : Image_Vector (1 .. Cats_Dir'Length * Num_Samples);
      Labels          : Integer_Array (1 .. Cats_Dir'Length * Num_Samples);
      Image_File_Dir  : String_9;
   begin
      Put_Line (Routine_Name & "reading training files.");
      for cat in Cats_Dir'Range loop
         Image_File_Dir := Cats_Dir (cat);
         --           Put_Line (Routine_Name & "reading " & String (Image_File_Dir));
         for img in 0 .. Num_Samples - 1 loop
            --              Put_Line (Routine_Name & "reading image " &
            --                          Integer'Image (cat + img));
            declare
               Image_Data  : constant Unsigned_8_Array_3D
                 := Get_Picture (Train_Directory & String (Image_File_Dir) &
                                   "/images/" & String (Image_File_Dir) & "_" &
                                   Trim (Integer'Image (img), Both) & ".JPEG",
                                 False);
            begin
               --                 Put_Line (Routine_Name & "cat + img " &
               --                             Integer'Image (cat + img));
               --                 Put_Line (Routine_Name & "Images length " &
               --                             Integer'Image (Images'Length));
               --                 Print_Matrix_Dimensions (Routine_Name & "Image_Data",
               --                                          Image_Data);
               Images (cat + img) := Image_Array (Image_Data);
            end;
            Labels (cat + img) := Label;
         end loop;
      end loop;
      Put_Line (Routine_Name & "training files read.");

      Train_Test_Split (Images, Labels, Train_X, Train_Y, Test_X, Test_Y);

   end Read_Cats;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X                     : Image_Vector; Y : Integer_Array;
      Train_X               : out Image_Vector; Train_Y : out Integer_Array;
      Test_X                : out Image_Vector; Test_Y : out Integer_Array) is
      Routine_Name : constant String := "Support_15A.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
      Train_Size   : constant Positive := Train_X'Length;
      Image_In     : Image_Array;
      Image_Out    : Image_Array;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for img in Train_X'Range loop
         Image_In := X (img);
         for row in Image_In'Range loop
            for col in Image_In'Range (2) loop
               for rgb in Image_In'Range (3) loop
                  Image_Out (row, col, rgb) := Image_In (row, col, rgb);
               end loop;
            end loop;
         end loop;
         Train_X (img) := Image_Out;
         Train_Y (img) := Y (img);
      end loop;

      for img in Test_X'Range loop
         Image_In := X (Train_Size + img);
         for row in Image_In'Range loop
            for col in Image_In'Range (2) loop
               for rgb in Image_In'Range (3) loop
                  Image_Out (row, col, rgb) :=
                    Image_In (row, col, rgb);
               end loop;
            end loop;
         end loop;
         Test_X (img) := Image_Out;
         Test_Y (img) := Y (Train_Size + img);
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------

end Support_15A;
