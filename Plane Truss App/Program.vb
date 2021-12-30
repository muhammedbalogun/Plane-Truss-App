Imports System

Module Module1

    Sub Main()
        Dim M, NJ, NR, NRJ, J, NBI, K, I As Integer
        Dim E, XCL, YCL As Double
        Dim NDJ As Integer = 2
        Dim MD As Integer = 2 * NDJ
        Dim NB As Integer = 0
        Console.WriteLine("KING FAHD UNIVERSITY OF PETROLEUM & MINERALS")
        Console.WriteLine("Civil & Environmental Engineering Department")
        Console.WriteLine("CE 511 - Advance Structural Analysis and Vibrations")
        Console.WriteLine("")
        Console.WriteLine("Course Instructor:")
        Console.WriteLine("Dr Saheed Adekunle")
        Console.WriteLine("")
        Console.WriteLine("Done By:")
        Console.WriteLine("Saeed Mohammed Al-Houri  ID 201431280")
        Console.WriteLine("Muhammed Olawale Balogun  ID 202110150")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("###---- PLANE TRUSS PROGRAM ----###")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("#-- STRUCTURAL PARAMETERS --#")
        Console.WriteLine("")
        Console.WriteLine("Enter the number of members")
        M = Console.ReadLine()
        Dim JJ(M - 1), JK(M - 1) As Integer
        Dim AX(M - 1), EL(M - 1), CX(M - 1), CY(M - 1) As Double
        Console.WriteLine("Enter the number of joints")
        NJ = Console.ReadLine()
        Dim ND As Integer
        ND = NDJ * NJ
        Dim JRL(ND - 1), ID(ND - 1) As Integer
        Dim x(NJ - 1), y(NJ - 1) As Double
        Console.WriteLine("Enter the number of support restraints")
        NR = Console.ReadLine()
        Dim N As Integer = ND - NR
        Console.WriteLine("Enter the number of restrained joints")
        NRJ = Console.ReadLine()
        Console.WriteLine("Enter the modulus of elasticity E")
        E = Console.ReadLine()
        Console.WriteLine("")

        Console.WriteLine("")
        Console.WriteLine("#-- JOINT COORDINATES --#")
        Console.WriteLine("")
        K = 0
        Do While K <= NJ - 1
            J = K + 1
            Console.WriteLine("JOINT " & J)
            Console.WriteLine("")
            Console.WriteLine(" X Coordinates for joint " & J)
            x(K) = Console.ReadLine()
            Console.WriteLine(" Y Coordinates for joint " & J)
            y(K) = Console.ReadLine()
            Console.WriteLine("")
            K = K + 1
        Loop
        Console.WriteLine("")
        Console.WriteLine("#-- MEMBER INFORMATION --#")
        Console.WriteLine("")
        J = 0
        Do While J <= M - 1
            K = J + 1
            Console.WriteLine("MEMBER " & K)
            Console.WriteLine("")
            Console.WriteLine(" J joint for member " & K)
            JJ(J) = Console.ReadLine()
            Console.WriteLine(" K joint for member " & K)
            JK(J) = Console.ReadLine()
            Console.WriteLine(" Cross-sectional area of member " & K)
            AX(J) = Console.ReadLine()
            NBI = NDJ * (Math.Abs(JK(J) - JJ(J)) + 1)
            If NBI > NB Then NB = NBI
            XCL = x(JK(J) - 1) - x(JJ(J) - 1)
            YCL = y(JK(J) - 1) - y(JJ(J) - 1)
            EL(J) = Math.Sqrt(XCL * XCL + YCL * YCL)
            CX(J) = XCL / EL(J)
            CY(J) = YCL / EL(J)
            Console.WriteLine("")
            J = J + 1
        Loop
        Console.WriteLine("")
        Console.WriteLine("#-- THE JOINT RESTRAINTS --#")
        Console.WriteLine("")
        J = 0
        Do While J <= NRJ - 1
            Console.WriteLine(" THE RESTRAINT JOINT # ")
            K = Console.ReadLine()
            Console.WriteLine("")
            Console.WriteLine(" The translation in the x direction for joint " & K)
            JRL(2 * (K - 1)) = Console.ReadLine()
            Console.WriteLine(" The translation in the y direction for joint " & K)
            JRL(2 * (K - 1) + 1) = Console.ReadLine()
            Console.WriteLine("")
            J = J + 1
        Loop
        Dim N1 As Integer = 0
        J = 0
        Do While J <= ND - 1
            N1 = N1 + JRL(J)
            If JRL(J) > 0 Then
                GoTo LANE1
            Else
                ID(J) = J - N1
                GoTo LANE2
            End If
LANE1:
            ID(J) = N - 1 + N1
LANE2:
            J = J + 1
        Loop

        'START OF STIFF2

        Dim K1, K2, J1, J2, I1, I2, IR, IC, ITEM, IM(11) As Integer
        Dim SCM, SMS(3, 3), SFF(N - 1, NB - 1) As Double
        I = 0
        Do While I <= M - 1
            SCM = E * AX(I) / EL(I)
            SMS(0, 0) = SCM * CX(I) * CX(I)
            SMS(0, 1) = SCM * CX(I) * CY(I)
            SMS(0, 2) = -SMS(0, 0)
            SMS(0, 3) = -SMS(0, 1)
            SMS(1, 1) = SCM * CY(I) * CY(I)
            SMS(1, 2) = -SMS(0, 1)
            SMS(1, 3) = -SMS(1, 1)
            SMS(2, 2) = SMS(0, 0)
            SMS(2, 3) = SMS(0, 1)
            SMS(3, 3) = SMS(1, 1)

            IM(0) = 2 * (JJ(I) - 1)
            IM(1) = 2 * (JJ(I) - 1) + 1
            IM(2) = 2 * (JK(I) - 1)
            IM(3) = 2 * (JK(I) - 1) + 1

            J = 0
            Do While J <= MD - 1
                I1 = IM(J)
                If JRL(I1) > 0 Then GoTo LANE5
                K = J
                Do While K <= MD - 1
                    I2 = IM(K)
                    If JRL(I2) > 0 Then GoTo LANE4
                    IR = ID(I1)
                    IC = ID(I2)
                    If IR < IC Then GoTo LANE3
                    ITEM = IR
                    IR = IC
                    IC = ITEM
LANE3:
                    IC = IC - IR
                    SFF(IR, IC) = SFF(IR, IC) + SMS(J, K)
LANE4:
                    K = K + 1
                Loop
LANE5:
                J = J + 1
            Loop
            I = I + 1
        Loop

        'START OF BANFAC

        Dim SUM, TEMP As Double
        If SFF(0, 0) <= 0.0 Then
            Console.WriteLine("SFF IS NOT POSITIVE DEFINITE ")
            GoTo LANELAST
        End If
        J = 1
        Do While J <= N - 1
            J1 = J - 1
            J2 = J - NB + 1
            If J2 < 0 Then J2 = 0   'CHECK
            If J1 = 0 Then GoTo LANE7
            I = 1
            Do While I <= J1
                I1 = I - 1
                If I1 < J2 Then GoTo LANE6
                SUM = SFF(I, J - I)
                K = J2
                Do While K <= I1
                    SUM = SUM - SFF(K, I - K) * SFF(K, J - K)
                    K = K + 1
                Loop
                SFF(I, J - I) = SUM
LANE6:
                I = I + 1
            Loop
LANE7:
            SUM = SFF(J, 0)
            K = J2
            Do While K <= J1
                TEMP = SFF(K, J - K) / SFF(K, 0)
                SUM = SUM - TEMP * SFF(K, J - K)
                SFF(K, J - K) = TEMP
                K = K + 1
            Loop
            If SUM <= 0.0 Then
                Console.WriteLine("SFF IS NOT POSITIVE DEFINITE ")
                GoTo LANELAST
            End If
            SFF(J, 0) = SUM
            J = J + 1
        Loop

        ' LOAD COMBINATIONS

        Dim LN As Integer = 0
        LN = LN + 1

        'START OF LDATA2

        Dim NLJ, NLM, LML(M - 1), JR As Integer
        Dim AJ(ND - 1), AML(3, M - 1), AC(ND - 1), AE(ND - 1), DF(N - 1) As Double
LANE8:
        Console.WriteLine("")
        Console.WriteLine("#-- LOADING INFORMATION --#")
        Console.WriteLine("")
        Console.WriteLine("Enter the number of joint loads")
        NLJ = Console.ReadLine()
        Console.WriteLine("Enter the number of member loads")
        NLM = Console.ReadLine()
        Console.WriteLine("")
        Console.WriteLine("ACTIONS AT THE JOINTS")
        Console.WriteLine("")
        If NLJ = 0 Then
            Console.WriteLine("NO ACTIONS AT THE JOINTS")
            GoTo LANE9
        End If
        J = 0
        Do While J <= NLJ - 1
            Console.WriteLine(" THE LOADED JOINT ")
            K = Console.ReadLine()
            Console.WriteLine("")
            Console.WriteLine(" The force in the x direction at joint " & K)
            AJ(2 * (K - 1)) = Console.ReadLine()
            Console.WriteLine(" The force in the y direction at joint " & K)
            AJ(2 * (K - 1) + 1) = Console.ReadLine()
            Console.WriteLine("")
            J = J + 1
        Loop
LANE9:
        Console.WriteLine("ACTIONS AT THE ENDS OF RESTRAINED MEMBERS DUE TO LOADS")
        Console.WriteLine("")
        If NLM = 0 Then
            Console.WriteLine("NO ACTIONS AT MEMBERS")
            GoTo LANE10
        End If
        J = 0
        Do While J <= NLM - 1
            Console.WriteLine(" THE LOADED MEMBER #")
            I = Console.ReadLine()
            Console.WriteLine("")
            Console.WriteLine("AT J JOINT")
            Console.WriteLine("")
            Console.WriteLine(" The force in the x directio of member " & I)
            AML(0, I - 1) = Console.ReadLine()
            Console.WriteLine(" The force in the y direction of member " & I)
            AML(1, I - 1) = Console.ReadLine()
            Console.WriteLine("")
            Console.WriteLine("AT K JOINT")
            Console.WriteLine("")
            Console.WriteLine(" The force in the x direction at K joint of member " & I)
            AML(2, I - 1) = Console.ReadLine()
            Console.WriteLine(" The force in the y direction at K joint of member " & I)
            AML(3, I - 1) = Console.ReadLine()
            Console.WriteLine("")
            LML(I - 1) = 1
            J = J + 1
        Loop
LANE10:
        'START OF LOADS2

        If NLM = 0 Then
            GoTo LANE12
        End If
        I = 0
        Do While I <= M - 1
            If LML(I) = 0 Then
                GoTo LANE11
            End If
            J1 = 2 * (JJ(I) - 1)
            J2 = 2 * (JJ(I) - 1) + 1
            K1 = 2 * (JK(I) - 1)
            K2 = 2 * (JK(I) - 1) + 1

            AE(J1) = AE(J1) - CX(I) * AML(0, I) + CY(I) * AML(1, I)
            AE(J2) = AE(J2) - CY(I) * AML(0, I) - CX(I) * AML(1, I)
            AE(K1) = AE(K1) - CX(I) * AML(2, I) + CY(I) * AML(3, I)
            AE(K2) = AE(K2) - CY(I) * AML(2, I) - CX(I) * AML(3, I)
LANE11:
            I = I + 1
        Loop
LANE12:
        J = 0
        Do While J <= ND - 1
            JR = ID(J)
            AC(JR) = AJ(J) + AE(J)
            J = J + 1
        Loop

        ' BANSOL

        I = 0
        Do While I <= N - 1
            J = I - NB + 1
            If I <= NB - 1 Then J = 0 'CHECK
            SUM = AC(I)
            K1 = I - 1
            If J > K1 Then GoTo LANE13
            K = J
            Do While K <= K1
                SUM = SUM - SFF(K, I - K) * DF(K)
                K = K + 1
            Loop
LANE13:
            DF(I) = SUM
            I = I + 1
        Loop
        I = 0
        Do While I <= N - 1
            DF(I) = DF(I) / SFF(I, 0)
            I = I + 1
        Loop
        I1 = 0
        Do While I1 <= N - 1
            I = N - I1 - 1
            J = I + NB - 1
            If J > N - 1 Then J = N - 1 'CHECK
            SUM = DF(I)
            K2 = I + 1
            If K2 > J Then GoTo LANE14
            K = K2
            Do While K <= J
                SUM = SUM - SFF(I, K - I) * DF(K)
                K = K + 1
            Loop
LANE14:
            DF(I) = SUM
            I1 = I1 + 1
        Loop

        ' RESUL6

        Dim JE As Integer
        Dim DJ(ND - 1), AMD(MD - 1), AM(3), AR(ND - 1) As Double
        J = N 'CHECK
        K = 0
        Do While K <= ND - 1
            JE = ND - K - 1
            If JRL(JE) = 0 Then GoTo LANE15
            DJ(JE) = 0.0
            GoTo LANE16
LANE15:
            J = J - 1
            DJ(JE) = DF(J)
LANE16:
            K = K + 1
        Loop
        Console.WriteLine("#-- THE RESULTS --#")
        Console.WriteLine("")
        Console.WriteLine("")
        Console.WriteLine("THE JOINT DISPLACEMENTS")
        Console.WriteLine("")
        J = 0
        Do While J <= NJ - 1
            K = J + 1
            Console.WriteLine("THE JOINT DISPLACEMENTS AT JOINT " & (J + 1))
            Console.WriteLine("")
            Console.WriteLine(" THE TRANSLATION DISPLACEMENT IN THE X DIRECTION   " & DJ(2 * J))
            Console.WriteLine(" THE TRANSLATION DISPLACEMENT IN THE Y DIRECTION   " & DJ(2 * J + 1))
            Console.WriteLine("")
            J = J + 1
        Loop
        Console.WriteLine("THE MEMBER END-ACTIONS")
        Console.WriteLine("")
        I = 0
        Do While I <= M - 1
            J1 = 2 * (JJ(I) - 1)
            J2 = 2 * (JJ(I) - 1) + 1
            K1 = 2 * (JK(I) - 1)
            K2 = 2 * (JK(I) - 1) + 1
            SCM = E * AX(I) / EL(I)
            AMD(0) = SCM * ((DJ(J1) - DJ(K1)) * CX(I) + (DJ(J2) - DJ(K2)) * CY(I))
            AMD(1) = 0.0
            AMD(2) = -AMD(0)
            AMD(3) = 0.0
            J = 0
            Do While J <= MD - 1
                AM(J) = AML(J, I) + AMD(J)
                J = J + 1
            Loop
            Console.WriteLine("THE MEMBER END-ACTIONS FOR MEMBER " & (I + 1))
            Console.WriteLine("")
            Console.WriteLine("AT J JOINT")
            Console.WriteLine(" THE FORCE IN THE X DIRECTION   " & AM(0))
            Console.WriteLine(" THE FORCE IN THE Y DIRECTION   " & AM(1))
            Console.WriteLine("")
            Console.WriteLine("AT K JOINT")
            Console.WriteLine(" THE FORCE IN THE X DIRECTION   " & AM(2))
            Console.WriteLine(" THE FORCE IN THE Y DIRECTION   " & AM(3))
            Console.WriteLine("")
            If JRL(J1) = 1 Then
                AR(J1) = AR(J1) + CX(I) * AMD(0)
            End If
            If JRL(J2) = 1 Then
                AR(J2) = AR(J2) + CY(I) * AMD(0)
            End If
            If JRL(K1) = 1 Then
                AR(K1) = AR(K1) + CX(I) * AMD(2)
            End If
            If JRL(K2) = 1 Then
                AR(K2) = AR(K2) + CY(I) * AMD(2)
            End If
            I = I + 1
        Loop
        J = 0
        Do While J <= ND - 1
            If JRL(J) = 0 Then GoTo LANE17
            AR(J) = AR(J) - AJ(J) - AE(J)
LANE17:
            J = J + 1
        Loop
        Console.WriteLine("THE SUPPORT REACTIONS")
        Console.WriteLine("")
        J = 0
        Do While J <= NJ - 1
            J1 = 2 * J
            J2 = 2 * J + 1
            N1 = JRL(J1) + JRL(J2)
            If N1 = 0 Then GoTo LANE18
            Console.WriteLine("THE SUPPORT REACTIONS FOR JOINT " & (J + 1))
            Console.WriteLine("")
            Console.WriteLine(" THE FORCE IN THE X DIRECTION   " & AR(J1))
            Console.WriteLine(" THE FORCE IN THE Y DIRECTION   " & AR(J2))
            Console.WriteLine("")
LANE18:
            J = J + 1
        Loop
LANELAST:
        Console.ReadLine()
        Console.ReadLine()
    End Sub

End Module
