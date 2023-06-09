# Generated by Selenium IDE
import pytest
import time
import json
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
path_to_driver = 'C:\Program Files (x86)\chromedriver.exe'

class TestTryingthingsout():
  def setup_method(self, method):
    self.driver = webdriver.Chrome(method)
    self.vars = {}

  def teardown_method(self, method):
    self.driver.quit()

  def test_tryingthingsout(self):
    self.driver.get("http://14.139.60.146/DownloadRawData/Download/RawData/DownloadRawData.aspx")
    self.driver.set_window_size(1536, 824)
    self.driver.find_element(By.ID, "drpAcademicYr").click()
    dropdown = self.driver.find_element(By.ID, "drpAcademicYr")
    dropdown.find_element(By.XPATH, "//option[. = '2010-11']").click()
    element = self.driver.find_element(By.LINK_TEXT, "Facilities")
    actions = ActionChains(self.driver)
    actions.move_to_element(element).perform()
    element = self.driver.find_element(By.CSS_SELECTOR, "body")
    actions = ActionChains(self.driver)
    actions.move_to_element(element, 0, 0).perform()
    self.driver.find_element(By.ID, "drpState").click()
    dropdown = self.driver.find_element(By.ID, "drpState")
    dropdown.find_element(By.XPATH, "//option[. = 'BIHAR']").click()
    self.driver.find_element(By.ID, "btnSearch").click()
    element = self.driver.find_element(By.LINK_TEXT, "Facilities")
    actions = ActionChains(self.driver)
    actions.move_to_element(element).perform()
    element = self.driver.find_element(By.CSS_SELECTOR, "body")
    actions = ActionChains(self.driver)
    actions.move_to_element(element, 0, 0).perform()
    element = self.driver.find_element(By.CSS_SELECTOR, ".ui-state-active > a")
    actions = ActionChains(self.driver)
    actions.move_to_element(element).perform()
    element = self.driver.find_element(By.CSS_SELECTOR, "body")
    actions = ActionChains(self.driver)
    actions.move_to_element(element, 0, 0).perform()
    self.driver.find_element(By.ID, "lnkDownloadGeneralData").click()
first_test = TestTryingthingsout()

first_test.setup_method(path_to_driver)
first_test.test_tryingthingsout()

#%%

first_test.teardown_method("kkn")
