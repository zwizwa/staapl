#lang staapl/pic18/dtc \ -*- forth -*-
provide-all
variable abc
: foo 123 ;   
: bar begin foo abc again ;

    