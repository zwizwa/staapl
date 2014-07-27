#lang staapl/pic18/dtc \ -*- forth -*-
provide-all
: foo 123 ;   
: bar begin foo again ;
    