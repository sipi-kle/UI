import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Scanner;

public class Astar {

	public static void search(int[][] graph, int startNode, ArrayList<Integer> endNodes, int[] hCost,
			int[][] labirint) {
		LinkedList<Integer> open = new LinkedList<Integer>();
		boolean[] closed = new boolean[graph.length];
		int[] from = new int[graph.length];

		int[] gScore = new int[graph.length];
		int[] fScore = new int[graph.length];

		for (int i = 0; i < gScore.length; i++) {
			gScore[i] = Integer.MAX_VALUE;
			fScore[i] = Integer.MAX_VALUE;
		}

		gScore[startNode] = 0;
		fScore[startNode] = hCost[startNode];
		from[startNode] = -1;

		open.add(startNode);
		// System.out.println("Odpiram vozlisce " + startNode);

		while (!open.isEmpty()) {
			int minVal = Integer.MAX_VALUE;
			int minPos = 0;
			int curNode = 0;

			for (int i = 0; i < open.size(); i++) {
				int node = open.get(i);
				if (fScore[node] < minVal) {
					minVal = fScore[node];
					minPos = i;
					curNode = node;
				}
			}

			open.remove(minPos);
			closed[curNode] = true;
			// System.out.println("Zapiram vozlisce " + curNode);

			if (endNodes.contains(curNode)) {
				// System.out.println("Resitev A* v vozliscu " + curNode);
				System.out.print("Pot: " + Pot(curNode));
				while (true) {
					curNode = from[curNode];
					if (curNode != -1) {
						System.out.print(" <-- " + Pot(curNode));
						cena += cenaPoti(labirint, curNode);
						stPremikov++;
					} else
						break;
				}
				cena += 2;
				return;
			}

			for (int nextNode = 0; nextNode < graph[curNode].length; nextNode++) {
				if (graph[curNode][nextNode] > 0 && !closed[nextNode]) {
					if (!open.contains(nextNode))
						// System.out.println("Odpiram vozlisce " + nextNode);
						vozlisca++;

					open.add(nextNode);
					int dist = gScore[curNode] + graph[curNode][nextNode];

					if (dist < gScore[nextNode]) {
						from[nextNode] = curNode;
						gScore[nextNode] = dist;
						fScore[nextNode] = gScore[nextNode] + hCost[nextNode];
						// System.out.println("Posodabljam vozlisce " + nextNode);
					}
				}
			}
		}
	}

	public static int cena = 0;
	public static int vozlisca = 0;
	public static int stPremikov = 0;

	public static void main(String[] args) throws FileNotFoundException {
		String[] fileNames = { "labyrinth_1.txt", "labyrinth_2.txt", "labyrinth_3.txt", "labyrinth_4.txt",
				"labyrinth_5.txt", "labyrinth_6.txt", "labyrinth_7.txt", "labyrinth_8.txt", "labyrinth_9.txt",
				"labyrinth_10.txt", "labyrinth_11.txt", "labyrinth_12.txt" };
		for (int i = 0; i < fileNames.length; i++) {
			ArrayList<int[]> arr = new ArrayList<int[]>();
			File file = new File(fileNames[i]);
			Scanner sc = new Scanner(file);
			while (sc.hasNextLine()) {

				String[] line = sc.nextLine().split(",");
				int[] tmp = new int[line.length];
				for (int j = 0; j < line.length; j++) {
					tmp[j] = Integer.parseInt(line[j]);
				}
				arr.add(tmp);
			}
			sc.close();
			int[][] lab = new int[arr.size()][arr.get(0).length];
			for (int j = 0; j < lab.length; j++) {
				lab[j] = arr.get(j);
			}

			executeCommand(lab);
			System.out.println("------------------------" + fileNames[i]
					+ "--------------------------------------------------------");
		}
	}

	public static void executeCommand(int[][] labirint) {
		int[][] matrika = getN(labirint);
		int[] hCost = getHcost(labirint, getEnds(labirint));

		Astar.search(matrika, getStart(labirint), getEnds(labirint), hCost, labirint);
		System.out.println("\nCena poti: " + cena);
		System.out.println("Obdelana vozlisca: " + vozlisca);
		System.out.println("Stevilo premikov na najdeni poti: " + stPremikov);
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

	public static String Pot(int curNode) {
		for (int i = 0; i < index.length; i++) {
			for (int y = 0; y < index[i].length; y++) {
				if (index[i][y] == curNode) {
					return ("[" + i + "," + y + "]");
				}
			}
		}
		return null;
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

	public static int[] getHcost(int[][] labirint, ArrayList<Integer> endNodes) {
		ArrayList<Integer> hCost = new ArrayList<Integer>();
		for (int i = 0; i < index.length; i++) {
			for (int y = 0; y < index[i].length; y++) {
				if (index[i][y] >= 0) {
					hCost.add(getShortest(endNodes, i, y));
				}
			}
		}
		return convertIntegers(hCost);
	}

	public static int getShortest(ArrayList<Integer> endNodes, int i, int y) {
		int Shortest = Integer.MAX_VALUE;
		int ei = 0;
		int ey = 0;
		int distance = 0;
		for (Object end : endNodes.toArray()) {
			for (int ii = 0; ii < index.length; ii++) {
				for (int yy = 0; yy < index[i].length; yy++) {
					if (index[ii][yy] == (int) end && index[ii][yy] != -1) {
						ei = ii;
						ey = yy;
						distance = Math.abs(i - ei) + Math.abs(y - ey);
						if (distance < Shortest) {
							Shortest = distance;
						}
						break;
					}
				}
			}
		}
		return Shortest;
	}

	public static int[] convertIntegers(ArrayList<Integer> integers) {
		int[] ret = new int[integers.size()];
		for (int i = 0; i < ret.length; i++) {
			ret[i] = integers.get(i).intValue();
		}
		return ret;
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
						matrika[currindex][foundindex] = labirint[i + 1][y];
					}
				} catch (Exception e) {
				}
				try {
					if (labirint[i - 1][y] != -1) {
						currindex = index[i][y];
						foundindex = index[i - 1][y];
						matrika[currindex][foundindex] = labirint[i - 1][y];
					}
				} catch (Exception e) {
				}
				try {
					if (labirint[i][y + 1] != -1) {
						currindex = index[i][y];
						foundindex = index[i][y + 1];
						matrika[currindex][foundindex] = labirint[i][y + 1];
					}
				} catch (Exception e) {
				}
				try {
					if (labirint[i][y - 1] != -1) {
						currindex = index[i][y];
						foundindex = index[i][y - 1];
						matrika[currindex][foundindex] = labirint[i][y - 1];
					}
				} catch (Exception e) {
				}

			}
		}
		for (int i = 0; i < matrika.length; i++) {
			for (int y = 0; y < matrika[i].length; y++) {
				if (matrika[i][y] == -3 || matrika[i][y] == -2) {
					matrika[i][y] = 1;
				}
			}
		}

		return matrika;
	}

}
