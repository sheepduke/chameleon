(in-package chameleon/tests)

(deftest test-defconfig
  (defconfig
    (myconfig "value1"))

  (testing "Test configuration access functions"
    (ok (fboundp 'myconfig)
        "Expect access function to be defined")
    (ok (equal (myconfig) "value1")
        "Expect default value is correct")
  (setf (myconfig) "value2")
  (ok (equal (myconfig) "value2")
      "Expect value got changed successfully"))

  (testing "Test profile related functions"
    (ok (fboundp 'profiles)
        "Expect PROFILES function got defined")
    (ok (equal (profiles) '(nil))
        "Expect only NIL profile got defined")
    (ok (fboundp 'active-profile)
        "Expect ACTIVE-PROFILE function got defined")
    (ok (equal (active-profile) nil)
        "Expect current profile is set to NIL"))

  (testing "Test other functions"
    (ok (equal (hash-table-count *configs*) 1)
        "Expect only 1 item exists in the configuration set")))

(deftest test-defprofile
  (defconfig
    (myconfig "value1"))
  
  (defprofile :develop
    (myconfig "value3"))
  
  (testing "Test profile definition"
    (ok (gethash :develop *configs*)
        "Expect newly defined profile exists")
    (ok (equal (active-profile) nil)
        "Expect currently profile is NIL"))
  (testing "Test newly defined profile"
    (ok (equal (myconfig) "value1")
        "Expect configuration equals to the default value")
    (setf (active-profile) :develop)
    (ok (equal (myconfig) "value3")
        "Expect configuration equals to the new value")))
