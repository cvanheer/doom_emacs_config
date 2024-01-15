;;; elfeed_score.el -*- lexical-binding: t; -*-

((version 10)
 ("title"
  (:text "probabilistic" :value 10 :type s)
  (:text "learning" :value -50 :type s)
  (:text "change point" :value -50 :type s))
 ("content")
 ("title-or-content"
  (:text "Learning" :title-value 50 :content-value 50 :type s)
  (:text "Task Goals" :title-value 50 :content-value 50 :type s)
  (:text "Probabilistic" :title-value 100 :content-value 50 :type S)
  (:text "change point" :title-value 100 :content-value 50 :type S))
 ("tag")
 ("authors"
  (:text "Wei Ji Ma" :value 200 :type w)
  (:text "Matthew Nassar" :value 200 :type w)
  (:text "Steve Fleming" :value 200 :type w)
  (:text "Florent Meyniel" :value 200 :type w))
 ("feed")
 ("link")
 ("udf")
 (mark nil)
 ("adjust-tags"))
