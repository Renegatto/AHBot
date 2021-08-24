{-# LANGUAGE ExplicitNamespaces #-}
module ArtHistory.Languages.Language 
  ( type AppL
  , subscriptionEvents
  , unsolvedQuiz
  , quizConfig
  , pushEvents
  , randomQuizSet
  , sendMessage
  , QuizSet
  , Result ) where

import ArtHistory.Languages.Definitions
  ( Result
  ,  QuizSet
  ,  subscriptionEvents
  ,  unsolvedQuiz
  ,  quizConfig
  ,  pushEvents
  ,  sendMessage
  ,  randomQuizSet )
  
import ArtHistory.Languages.Interpreters 
  (type AppL)   