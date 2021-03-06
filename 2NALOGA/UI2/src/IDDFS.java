
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.Stack;

public class IDDFS {
	
	public static void search(int[][] graph, int startNode, ArrayList<Integer> endNodes, int[][] labirint)
	{
		for (int depthLimit = 0; depthLimit < graph.length; depthLimit++)
		{
			//System.out.println("Globina iskanja je " + depthLimit);
			
			
			boolean[] marked = new boolean[graph.length];
			int[] from = new int[graph.length];
			
			
			Stack<Integer> stack = new Stack<Integer>();
		
			from[startNode] = -1;
			marked[startNode] = true;
			stack.push(startNode);
			
			//System.out.println("Polagam na sklad vozlisce " + startNode);
			vozlisca++;
			
			while(!stack.isEmpty())
			{
				int curNode = stack.peek();
	
				if (endNodes.contains(curNode))
				{
					//System.out.println("Resitev IDDFS v vozliscu " + curNode);
					System.out.print("Pot: " + Pot(curNode));
					
					
					while (true)
					{
						curNode = from[curNode];
						if (curNode != -1) {
							System.out.print(" <-- " + Pot(curNode));
							cena += cenaPoti(labirint, curNode);
							stPremikov++;
						}
						else
							break;
					}
					cena += 2;
					return;
				}
	
				boolean found = false;
				if (stack.size() <= depthLimit)
				{
					// najdi neobiskanega naslednjika
					
					for (int nextNode = 0; nextNode < graph[curNode].length; nextNode++)
					{
						if (graph[curNode][nextNode] == 1 && !marked[nextNode])
						{
							marked[nextNode] = true;
							from[nextNode] = curNode;
							stack.push(nextNode);
							
							//System.out.println("Polagam na sklad vozlisce " + nextNode);
							vozlisca++;
							found = true;
							break;
						}
					}
				}
				
				if (!found)
				{
					stack.pop();
					//System.out.println("Odstranjum s sklada vozlisce " + curNode);
				}
			}
			
			
			//System.out.println("-----------------------------------------------------------");
		}
	}
	
	public static int cena = 0;
	public static int vozlisca = 0;
	public static int stPremikov = 0;
	
	public static void main(String[] args) throws FileNotFoundException {
		String[] fileNames= {"labyrinth_1.txt","labyrinth_2.txt","labyrinth_3.txt","labyrinth_4.txt","labyrinth_5.txt","labyrinth_6.txt","labyrinth_7.txt","labyrinth_8.txt","labyrinth_9.txt","labyrinth_10.txt","labyrinth_11.txt","labyrinth_12.txt"};
	for(int i=0;i<fileNames.length;i++) {
		ArrayList<int[]> arr =new ArrayList<int[]>();
		File file = new File(fileNames[i]);
		Scanner sc=new Scanner(file);
		while (sc.hasNextLine()) {
			
			String[] line=sc.nextLine().split(",");
			int[] tmp=new int[line.length];
			for(int j=0;j<line.length;j++) {
				tmp[j]=Integer.parseInt(line[j]);
			}
			arr.add(tmp);
		}
		sc.close();
		int[][] lab=new int[arr.size()][arr.get(0).length];
		for(int j=0;j<lab.length;j++) {
			lab[j]=arr.get(j);
		}
		
		executeCommand(lab);
		System.out.println("------------------------"+fileNames[i]+"--------------------------------------------------------");
	}
		
	}
	
	public static void executeCommand(int[][] labirint) {
		int[][] matrika = getN(labirint);
		
		IDDFS.search(matrika, getStart(labirint), getEnds(labirint), labirint);
		System.out.println("\nCena poti: " + cena);
		System.out.println("Obdelana vozlisca: " + vozlisca);
		System.out.println("Stevilo premikov na najdeni poti: " + stPremikov);
	}
	
	public static String Pot(int curNode) {
		for (int i = 0; i < index.length; i++) {
			for (int y = 0; y < index[i].length; y++) {
				if (index[i][y] == curNode) {
					return ("[" + i +"," + y + "]");
				}
			}
		}
		return null;
	}
	
	public static int cenaPoti(int[][] labirint, int curNode) {
		int cost = 0;
		for (int i = 0; i < index.length; i++) {
			for (int y = 0; y < index[i].length; y++) {
				if (index[i][y] == curNode) {
					cost += labirint[i][y];
					break;
				}
			}
		}
		return cost;
	}
	
	public static int getStart(int[][] labirint) {
		for (int i = 0; i < labirint.length; i++) {
			for (int y = 0; y < labirint[i].length; y++) {
				if (labirint[i][y] == -2) {
					return index[i][y];
				}
			}
		}
		return -1;
	}

	public static ArrayList<Integer> getEnds(int[][] labirint) {

		ArrayList<Integer> ends = new ArrayList<Integer>();

		for (int i = 0; i < labirint.length; i++) {
			for (int y = 0; y < labirint[i].length; y++) {
				if (labirint[i][y] == -3) {
					ends.add(index[i][y]);
				}
			}
		}
		return ends;
	}

	public static int[][] index;

	public static int getSize(int[][] labirint) {
		index = new int[labirint.length][labirint[0].length];
		int size = 0;
		for (int i = 0; i < labirint.length; i++) {
			for (int y = 0; y < labirint[i].length; y++) {
				if (labirint[i][y] != -1) {
					index[i][y] = size;
					size++;
				} else {
					index[i][y] = -1;
				}
			}
		}
		return size;
	}

	public static int[][] getN(int[][] labirint) {

		int currindex;
		int foundindex;

		int size = getSize(labirint);
		int[][] matrika = new int[size][size];
		for (int i = 0; i < labirint.length; i++) {
			for (int y = 0; y < labirint[0].length; y++) {
				try {
					if (labirint[i + 1][y] != -1) {
						currindex = index[i][y];
						foundindex = index[i + 1][y];
						matrika[currindex][foundindex] = 1;
					}
				} catch (Exception e) {
				}
				try {
					if (labirint[i - 1][y] != -1) {
						currindex = index[i][y];
						foundindex = index[i - 1][y];
						matrika[currindex][foundindex] = 1;
					}
				} catch (Exception e) {
				}
				try {
					if (labirint[i][y + 1] != -1) {
						currindex = index[i][y];
						foundindex = index[i][y + 1];
						matrika[currindex][foundindex] = 1;
					}
				} catch (Exception e) {
				}
				try {
					if (labirint[i][y - 1] != -1) {
						currindex = index[i][y];
						foundindex = index[i][y - 1];
						matrika[currindex][foundindex] = 1;
					}
				} catch (Exception e) {
				}

			}
		}
		return matrika;
	}

}

