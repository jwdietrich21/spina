FasdUAS 1.101.10   ��   ��    k             l     ��  ��    @ : This Script copies preferences and reference values files     � 	 	 t   T h i s   S c r i p t   c o p i e s   p r e f e r e n c e s   a n d   r e f e r e n c e   v a l u e s   f i l e s   
  
 l     ��  ��    < 6 that are named following the old naming convention to     �   l   t h a t   a r e   n a m e d   f o l l o w i n g   t h e   o l d   n a m i n g   c o n v e n t i o n   t o      l     ��  ��    H B new files labeled according to a more specific naming convention.     �   �   n e w   f i l e s   l a b e l e d   a c c o r d i n g   t o   a   m o r e   s p e c i f i c   n a m i n g   c o n v e n t i o n .      l     ��������  ��  ��        i         I      �� ���� 0 
fileexists 
FileExists   ��  o      ���� 0 thefile theFile��  ��    O         Z      ��    I   �� !��
�� .coredoexbool       obj  ! 4    �� "
�� 
file " o    ���� 0 thefile theFile��    L     # # m    ��
�� boovtrue��     L     $ $ m    ��
�� boovfals  l     %���� % m      & &�                                                                                  sevs   alis    �  
G5 Leopard                 �]��H+     �System Events.app                                                �q�c        ����  	                CoreServices    �]��      ��C       �   Q   P  8G5 Leopard:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p   
 G 5   L e o p a r d  -System/Library/CoreServices/System Events.app   / ��  ��  ��     ' ( ' l     ��������  ��  ��   (  ) * ) l     +���� + r      , - , m      . . � / /  : - n      0 1 0 1    ��
�� 
txdl 1 1    ��
�� 
ascr��  ��   *  2 3 2 l   	 4���� 4 r    	 5 6 5 m     7 7 � 8 8   6 o      ���� 0 messagereply messageReply��  ��   3  9 : 9 l     ��������  ��  ��   :  ; < ; l  
  =���� = r   
  > ? > m   
  @ @ � A A  n e t . s f . s p i n a ? o      ���� "0 spina_global_id SPINA_GLOBAL_ID��  ��   <  B C B l    D���� D r     E F E m     G G � H H " n e t . s f . s p i n a . t h y r F o      ���� ,0 spina_thyr_global_id SPINA_THYR_GLOBAL_ID��  ��   C  I J I l     ��������  ��  ��   J  K L K l   ! M���� M r    ! N O N c     P Q P b     R S R b     T U T l    V���� V I   �� W X
�� .earsffdralis        afdr W m    ��
�� afdmpref X �� Y��
�� 
rtyp Y l    Z���� Z m    ��
�� 
ctxt��  ��  ��  ��  ��   U o    ���� "0 spina_global_id SPINA_GLOBAL_ID S m     [ [ � \ \  . x m l Q m    ��
�� 
TEXT O o      ���� (0 oldpreferencesfile oldPreferencesFile��  ��   L  ] ^ ] l  " 5 _���� _ r   " 5 ` a ` c   " 1 b c b b   " / d e d b   " + f g f l  " ) h���� h I  " )�� i j
�� .earsffdralis        afdr i m   " #��
�� afdmpref j �� k��
�� 
rtyp k l  $ % l���� l m   $ %��
�� 
ctxt��  ��  ��  ��  ��   g o   ) *���� "0 spina_global_id SPINA_GLOBAL_ID e m   + . m m � n n  . r e f - r a n g e s . x m l c m   / 0��
�� 
TEXT a o      ���� 0 	oldrrfile 	oldRRFile��  ��   ^  o p o l     ��������  ��  ��   p  q r q l  6 I s���� s r   6 I t u t c   6 E v w v b   6 C x y x b   6 ? z { z l  6 = |���� | I  6 =�� } ~
�� .earsffdralis        afdr } m   6 7��
�� afdmpref ~ �� ��
�� 
rtyp  l  8 9 ����� � m   8 9��
�� 
ctxt��  ��  ��  ��  ��   { o   = >���� ,0 spina_thyr_global_id SPINA_THYR_GLOBAL_ID y m   ? B � � � � �  . x m l w m   C D��
�� 
TEXT u o      ���� (0 newpreferencesfile NewPreferencesFile��  ��   r  � � � l  J ] ����� � r   J ] � � � c   J Y � � � b   J W � � � b   J S � � � l  J Q ����� � I  J Q�� � �
�� .earsffdralis        afdr � m   J K��
�� afdmpref � �� ���
�� 
rtyp � l  L M ����� � m   L M��
�� 
ctxt��  ��  ��  ��  ��   � o   Q R���� ,0 spina_thyr_global_id SPINA_THYR_GLOBAL_ID � m   S V � � � � �  . r e f - r a n g e s . x m l � m   W X��
�� 
TEXT � o      ���� 0 	newrrfile 	NewRRFile��  ��   �  � � � l     ��������  ��  ��   �  � � � l  ^ � ����� � Z   ^ � � ��� � � I   ^ f�� ����� 0 
fileexists 
FileExists �  ��� � o   _ b���� (0 newpreferencesfile NewPreferencesFile��  ��   � r   i t � � � b   i r � � � m   i l � � � � � � P r e f e r e n c e s   f i l e   n o t   c o p i e d   a s   a   f i l e   w i t h   n e w   b u n d l e   I D   e x i s t e d .   � J   l q � �  ��� � o   l o��
�� 
ret ��   � o      ���� 0 messagereply messageReply��   � Z   w � � ��� � � I   w }�� ����� 0 
fileexists 
FileExists �  ��� � o   x y���� (0 oldpreferencesfile oldPreferencesFile��  ��   � k   � � � �  � � � O   � � � � � k   � � � �  � � � r   � � � � � I  � ��� � �
�� .coreclon****      � **** � 4   � ��� �
�� 
file � o   � ����� (0 oldpreferencesfile oldPreferencesFile � �� ���
�� 
alrp � m   � ���
�� boovfals��   � o      ���� 0 newfile newFile �  ��� � r   � � � � � n   � � � � � 4  � ��� �
�� 
citm � m   � ����� � o   � ��~�~ (0 newpreferencesfile NewPreferencesFile � l      ��}�| � n       � � � 1   � ��{
�{ 
pnam � l  � � ��z�y � c   � � � � � o   � ��x�x 0 newfile newFile � m   � ��w
�w 
alis�z  �y  �}  �|  ��   � l  � � ��v�u � m   � � � ��                                                                                  MACS   alis    l  
G5 Leopard                 �]��H+     �
Finder.app                                                       v��R��        ����  	                CoreServices    �]��      �R��       �   Q   P  1G5 Leopard:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p   
 G 5   L e o p a r d  &System/Library/CoreServices/Finder.app  / ��  �v  �u   �  ��t � r   � � � � � b   � � � � � m   � � � � � � � 2 P r e f e r e n c e s   f i l e   c o p i e d .   � J   � � � �  ��s � o   � ��r
�r 
ret �s   � o      �q�q 0 messagereply messageReply�t  ��   � r   � � � � � b   � � � � � m   � � � � � � � 4 P r e f e r e n c e s   f i l e   m i s s i n g .   � J   � � � �  ��p � o   � ��o
�o 
ret �p   � o      �n�n 0 messagereply messageReply��  ��   �  � � � l     �m�l�k�m  �l  �k   �  � � � l  �+ ��j�i � Z   �+ � ��h � � I   � ��g ��f�g 0 
fileexists 
FileExists �  ��e � o   � ��d�d 0 	newrrfile 	NewRRFile�e  �f   � r   � � � � � b   � � � � � o   � ��c�c 0 messagereply messageReply � m   � � � � � � � � R e f e r e n c e   r a n g e s   f i l e   n o t   c o p i e d   a s   a   f i l e   w i t h   n e w   b u n d l e   I D   e x i s t e d . � o      �b�b 0 messagereply messageReply�h   � Z   �+ � ��a � � I   � ��` ��_�` 0 
fileexists 
FileExists �  ��^ � o   � ��]�] 0 	oldrrfile 	oldRRFile�^  �_   � k   �! � �  � � � O   � � � � k   � � �  � � � r   � � � � I  � ��\ � �
�\ .coreclon****      � **** � 4   � ��[ �
�[ 
file � o   � ��Z�Z 0 	oldrrfile 	oldRRFile � �Y ��X
�Y 
alrp � m   � ��W
�W boovfals�X   � o      �V�V 0 newfile newFile �  ��U � r   � � � n   �  � 4 �T
�T 
citm m  
�S�S��  o  �R�R 0 	newrrfile 	NewRRFile � l     �Q�P n       1  �O
�O 
pnam l �N�M c   o  �L�L 0 newfile newFile m  �K
�K 
alis�N  �M  �Q  �P  �U   � l  � ��J�I m   � �		�                                                                                  MACS   alis    l  
G5 Leopard                 �]��H+     �
Finder.app                                                       v��R��        ����  	                CoreServices    �]��      �R��       �   Q   P  1G5 Leopard:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p   
 G 5   L e o p a r d  &System/Library/CoreServices/Finder.app  / ��  �J  �I   � 
�H
 r  ! b   o  �G�G 0 messagereply messageReply m   � : R e f e r e n c e   r a n g e s   f i l e   c o p i e d . o      �F�F 0 messagereply messageReply�H  �a   � r  $+ b  $) o  $%�E�E 0 messagereply messageReply m  %( � 2 R e f e r e n c e   r a n g e s   m i s s i n g . o      �D�D 0 messagereply messageReply�j  �i   �  l     �C�B�A�C  �B  �A    l ,?�@�? I ,?�>
�> .sysodlogaskr        TEXT o  ,-�=�= 0 messagereply messageReply �<
�< 
btns J  05   !�;! m  03"" �##  O K�;   �:$�9
�: 
dflt$ m  89�8�8 �9  �@  �?   %&% l     �7'(�7  ' ' !display dialog oldPreferencesFile   ( �)) B d i s p l a y   d i a l o g   o l d P r e f e r e n c e s F i l e& *�6* l     �5+,�5  + ' !display dialog NewPreferencesFile   , �-- B d i s p l a y   d i a l o g   N e w P r e f e r e n c e s F i l e�6       �4./0�4  . �3�2�3 0 
fileexists 
FileExists
�2 .aevtoappnull  �   � ****/ �1 �0�/12�.�1 0 
fileexists 
FileExists�0 �-3�- 3  �,�, 0 thefile theFile�/  1 �+�+ 0 thefile theFile2  &�*�)
�* 
file
�) .coredoexbool       obj �. � *�/j  eY fU0 �(4�'�&56�%
�( .aevtoappnull  �   � ****4 k    ?77  )88  299  ;::  B;;  K<<  ]==  q>>  �??  �@@  �AA �$�$  �'  �&  5  6 + .�#�" 7�! @�  G����� [�� m� �� ��� �� �������� � � ��"��
�	
�# 
ascr
�" 
txdl�! 0 messagereply messageReply�  "0 spina_global_id SPINA_GLOBAL_ID� ,0 spina_thyr_global_id SPINA_THYR_GLOBAL_ID
� afdmpref
� 
rtyp
� 
ctxt
� .earsffdralis        afdr
� 
TEXT� (0 oldpreferencesfile oldPreferencesFile� 0 	oldrrfile 	oldRRFile� (0 newpreferencesfile NewPreferencesFile� 0 	newrrfile 	NewRRFile� 0 
fileexists 
FileExists
� 
ret 
� 
file
� 
alrp
� .coreclon****      � ****� 0 newfile newFile
� 
citm
� 
alis
� 
pnam
� 
btns
� 
dflt�
 
�	 .sysodlogaskr        TEXT�%@���,FO�E�O�E�O�E�O���l �%�%�&E�O���l �%a %�&E` O���l �%a %�&E` O���l �%a %�&E` O*_ k+  a _ kv%E�Y S*�k+  ?a  )*a �/a fl E` O_ a i/_ a &a  ,FUOa !_ kv%E�Y a "_ kv%E�O*_ k+  �a #%E�Y O*_ k+  =a  +*a _ /a fl E` O_ a i/_ a &a  ,FUO�a $%E�Y 	�a %%E�O�a &a 'kva (ka ) *ascr  ��ޭ