--  Based on scipy/optimise/_minimize.py

--  with Ada.Text_IO; use Ada.Text_IO;

with L_BFGS_B;
with ML_Types;
with NL_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package body Opt_Minimise is

   function Optimize_Result_For_Equal_Bounds
     (Fun    : Multilayer_Perceptron.Loss_Grad_Access;
      Args   :  Multilayer_Perceptron.Loss_Grad_Args;
      Bounds : Opt_Constraints.Bounds_List)
       return Optimise.Optimise_Result;
   --  Standardize_Bounds is Python format specific
   function Remove_From_Bounds
     (Bounds : Opt_Constraints.Bounds_List; Index : Positive)
       return Opt_Constraints.Bounds_List;

   --  -------------------------------------------------------------------------

   function Check_Options return Boolean is
   begin
      return True;

   end Check_Options;

   --  -------------------------------------------------------------------------
   --  L45 Minimization of scalar function of one or more variables.
   --  Fun: The objective function to be minimized
   --  fun(x, *args) -> float
   --  x is a 1-D array with shape (n,) and ``args`` is a list of the
   --  fixed parameters needed to completely specify the function.
   --  x0 : ndarray, shape (n,) is an initial guess array of real elements of
   --  size (n,);  n is the number of independent variables.
   function Minimise (Fun         : Multilayer_Perceptron.Loss_Grad_Access;
                      Args        : Multilayer_Perceptron.Loss_Grad_Args;
                      X0          : Stochastic_Optimizers.Parameters_List;
                      Method      : Method_Type := No_Method;
                      Jac         : Num_Diff.FD_Methods := Num_Diff.FD_None;
                      --                         Hess        : Num_Diff.FD_Methods := Num_Diff.FD_None;
                      Bounds      : Opt_Constraints.Bounds_List :=
                        Opt_Constraints.Array_Bounds_Package.Empty_Vector;
                      Constraints : Minimise_Constraints_List :=
                        Minimise_Constraints_Package.Empty_List)
                       return Optimise.Optimise_Result is
      --                        Options     : Minimise_Options := No_Options)
      use Ada.Containers;
      use Minimise_Constraints_Package;
      use Num_Diff;
      --          Routine_Name : constant String := "Opt_Minimise.Minimise ";
      --  L prefix indicates local variable
      L_Method     : Method_Type := Method;
      L_Bounds     : Opt_Constraints.Bounds_List := Bounds;
      I_Fixed      : ML_Types.Integer_List;
      All_Fixed    : Boolean := True;
      --  L655 determine if finite differences are needed for any grad or jac
      FD_Needed    : Boolean := Jac /= Fd_Callable;
      Cons_Cursor  : Cursor := Constraints.First;
      Remove_Vars  : Boolean := False;
      Done         : Boolean := False;
      X_Fixed      : NL_Types.Float_List;
      Result       : Optimise.Optimise_Result  (Args.Num_Rows, 0, 0);
   begin
      Result.Fun := Fun (Args);
      --  L504
      if L_Method = No_Method then
         if not Constraints.Is_Empty then
            L_Method := Slsqp_Method;
         elsif not Bounds.Is_Empty then
            L_Method := L_BFGS_B_Method;
         else
            L_Method := BFGS_Method;
         end if;
      end if;

      --  L545 Check_Options is currently always True
      --  Intent is to check if the optional parameters are supported by the
      --  selected method.
      --  Check_Options covers L549 - L578
      if Check_Options then
         --  L581 - 599 Jac checks
         --  L602 - 619 set default tolerances
         --  L632
         if not Bounds.Is_Empty then
            --    bounds = standardize_bounds(bounds, x0, meth) Python specific
            if L_Method = Tnc_Method or L_Method = Slsqp_Method or
              L_Method = L_BFGS_B_Method then
               --  L645
               for index in Bounds.First_Index .. Bounds.Last_Index loop
                  if Bounds (index).Upper = Bounds (index).Lower then
                     I_Fixed.Append (index);
                  end if;
               end loop;
               All_Fixed := I_Fixed.Length = Bounds.Length;

               --  L667
               Done := All_Fixed;
               if All_Fixed then
                  return Optimize_Result_For_Equal_Bounds
                    (Fun, Args, Bounds);

               else
                  while Has_Element (Cons_Cursor) loop
                     --  L657
                     if Element (Cons_Cursor) /= Callable_Constraint then
                        FD_Needed := True;
                        --  L662
                        for index in I_Fixed.First_Index
                          .. I_Fixed.Last_Index loop
                           Remove_Vars := Remove_Vars or not I_Fixed.Is_Empty;
                        end loop;

                        Remove_Vars := Remove_Vars or FD_Needed or
                          Method = Tnc_Method;
                        --  L663
                        if Remove_Vars then
                           for index in reverse I_Fixed.First_Index
                             .. I_Fixed.Last_Index loop
                              --  Fixed variable indicated by equal upper and
                              --  lower bounds
                              X_Fixed.Append (L_Bounds.Element (index).Lower);
                              L_Bounds := Remove_From_Bounds (L_Bounds, index);
--                                declare
--                                   X01 : Stochastic_Optimizers.Parameters_Record
--                                     := X0.Element (index);
--                                begin
--                                   null;
--                                end;
                           end loop;
                        end if;
                     end if;
                     Next (Cons_Cursor);
                  end loop;

               end if;
            end if;
         end if;

         if not Done then
            --  L686
            case Method is
               when L_BFGS_B_Method =>
                  --  L623
                  return L_BFGS_B.Minimise_LBFGSB
                    (Fun => Fun, Args => Args, X0 => X0, Bounds => L_Bounds);
               when others => null;
            end case;
         end if;

         --  L729
         if Remove_Vars then
            declare
               New_Result : Optimise.Optimise_Result
                 (Result.X_Length + Natural (I_Fixed.Length + X_Fixed.Length),
                  Result.N_Coor, Result.N);
            begin
               New_Result.Fun := Result.Fun;
               New_Result.X := Result.X;
               New_Result.Jac := Result.Jac;
               New_Result.Hess := Result.Hess;
               New_Result.Hess_Inv := Result.Hess_Inv;
               New_Result.N_It := Result.N_It;
               New_Result.N_Fev := Result.N_Fev;
               New_Result.N_Jev := Result.N_Jev;
               New_Result.N_Hev := Result.N_Hev;
               New_Result.Status := Result.Status;
               New_Result.Success := Result.Success;

               return New_Result;
            end;
         end if;
      end if;  --  Check_Options

      return Result;

   end Minimise;

   --  -------------------------------------------------------------------------
   --  L977
   --     Loss_Grad_LBFGS
   --        (Self        : in out Multilayer_Perceptron.MLP_Classifier;
   --         Params      : Stochastic_Optimizers.Parameters_List;
   --         X           : Real_Float_Matrix;
   --         Y           : Boolean_Matrix;
   --         Activations : in out Real_Matrix_List;
   --         Gradients   : out Stochastic_Optimizers.Parameters_List)
   --         return Float;
   function Optimize_Result_For_Equal_Bounds
     (Fun    : Multilayer_Perceptron.Loss_Grad_Access;
      Args   :  Multilayer_Perceptron.Loss_Grad_Args;
      Bounds : Opt_Constraints.Bounds_List)
       return Optimise.Optimise_Result is
      Success : constant Boolean := True;
      X0      : Real_Float_Vector (1 .. Positive (Bounds.Length));
      --  L1013
      Result  : Optimise.Optimise_Result (0, 0, 0);
   begin
      for index in Bounds.First_Index .. Bounds.Last_Index loop
         X0 (index) := Bounds (index).Lower;
      end loop;

      Result.Fun := Fun (Args);
      Result.X := X0;
      Result.N_Fev := 1;
      Result.Success := Success;
      return Result;

   end Optimize_Result_For_Equal_Bounds;

   --  -------------------------------------------------------------------------

   function Remove_From_Bounds
     (Bounds : Opt_Constraints.Bounds_List; Index : Positive)
       return Opt_Constraints.Bounds_List is
      New_Bounds : Opt_Constraints.Bounds_List := Bounds;
   begin
      New_Bounds.Delete (Index);
      return New_Bounds;

   end Remove_From_Bounds;

   --  -------------------------------------------------------------------------

end Opt_Minimise;
