package mango123.util;

public class EulerStringUtils {
	//prevent initialization
	private EulerStringUtils() {}
	   
	public static boolean isPalindromic(String str) {
		for (int  i = -1, len = str.length() - 1, bound = str.length() / 2; ++i < bound;) {
			if (str.charAt(i) != str.charAt(len - i)) return false;
		}
	    return true;
	}
	
	public static String reverse(String str) {
		StringBuilder newStr = new StringBuilder(str.length());
	    for (int i = str.length(); --i >= 0;) {
	    	newStr.append(str.charAt(i));
	    }
	    return newStr.toString();
	}
}
